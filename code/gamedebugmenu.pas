{
  Copyright 2006-2022 Michalis Kamburelis.

  This file is part of "castle".

  "castle" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "castle" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "castle"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit GameDebugMenu;

interface

uses Classes,
  CastleUIState, CastleUIControls, CastleKeysMouse, CastleImages,
  GameGeneralMenu;

type
  TStateDebugMenu = class(TAbstractMenuState)
  strict private
    OldThemeWindow: String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
   function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateDebugMenu: TStateDebugMenu;

implementation

uses SysUtils,
  CastleUtils, CastleStringUtils, CastleTransform,
  CastleGLUtils, CastleMessages, GameWindow,
  CastleVectors, CastleWindow, GamePlay,
  CastleInputs, CastleCreatures, GameChooseMenu,
  CastleItems, CastleOnScreenMenu, GameVideoOptions, CastleSoundEngine,
  X3DNodes, CastleClassUtils, CastleGameNotifications,
  CastleLevels, CastleResources, CastleControls, CastleApplicationProperties;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TDebugMenu = class(TCastleGameMenu)
  strict private
    procedure ClickPlayerMenu(Sender: TObject);
    procedure ClickCreaturesMenu(Sender: TObject);
    procedure ClickItemsMenu(Sender: TObject);
    procedure ClickLevelsMenu(Sender: TObject);
    procedure ClickReloadResources(Sender: TObject);
    procedure ClickReloadResourceAnimation(Sender: TObject);
    procedure ClickRenderDebug(Sender: TObject);
    procedure ClickShadowVolumesRender(Sender: TObject);
    procedure ClickDebugRenderForLevelScreenshot(Sender: TObject);
    procedure ClickReloadSounds(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    RenderDebugToggle: TCastleOnScreenMenuItemToggle;
    ShadowVolumesRenderToggle: TCastleOnScreenMenuItemToggle;
    DebugRenderForLevelScreenshotToggle: TCastleOnScreenMenuItemToggle;
    constructor Create(AOwner: TComponent); override;
  end;

  TDebugPlayerMenu = class(TCastleGameMenu)
  strict private
    procedure ClickInfiniteLife(Sender: TObject);
    procedure ClickFly(Sender: TObject);
    procedure ClickReloadPlayerXml(Sender: TObject);
    procedure ClickBack(Sender: TObject);
    procedure RotationHorizontalSpeedChanged(Sender: TObject);
    procedure RotationVerticalSpeedChanged(Sender: TObject);
    procedure PlayerSpeedChanged(Sender: TObject);
  public
    RotationHorizontalSpeedSlider: TCastleFloatSlider;
    RotationVerticalSpeedSlider: TCastleFloatSlider;
    PlayerSpeedSlider: TCastleFloatSlider;
    constructor Create(AOwner: TComponent); override;
  end;

  TDebugCreaturesMenu = class(TCastleGameMenu)
  strict private
    procedure KillAllCore(const IncludeStill: boolean);
    procedure ClickKillAll(Sender: TObject);
    procedure ClickKillAllNonStill(Sender: TObject);
    procedure ClickAddCreature(Sender: TObject);
    procedure ClickDebugTimeStopForCreatures(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    DebugTimeStopForCreaturesToggle: TCastleOnScreenMenuItemToggle;
    constructor Create(AOwner: TComponent); override;
  end;

  TDebugItemsMenu = class(TCastleGameMenu)
  strict private
    procedure ClickGiveAll(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDebugLevelMenu = class(TCastleGameMenu)
  strict private
    procedure ClickChangeLevel(Sender: TObject);
    procedure ClickRestart(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  DebugMenu: TDebugMenu;
  DebugPlayerMenu: TDebugPlayerMenu;
  DebugCreaturesMenu: TDebugCreaturesMenu;
  DebugLevelMenu: TDebugLevelMenu;
  DebugItemsMenu: TDebugItemsMenu;

{ utility -------------------------------------------------------------------- }

function ChooseResource(out Resource: T3DResource;
  const OnlyCreatures: boolean): boolean;
var
  S: TStringList;
  I, ResultIndex: Integer;
begin
  S := TStringList.Create;
  try
    for I := 0 to Resources.Count - 1 do
      if (not OnlyCreatures) or (Resources[I] is TCreatureResource) then
        S.AddObject(Format('Resource %s (%d users)',
          [Resources[I].Name, Resources[I].UsageCount]), Resources[I]);
    S.Append('Cancel');
    ResultIndex := ChooseByMenu(S);
    Result := ResultIndex <> S.Count - 1;
    if Result then
      Resource := S.Objects[ResultIndex] as T3DResource;
  finally S.Free end;
end;

{ TDebugMenu ------------------------------------------------------------ }

constructor TDebugMenu.Create(AOwner: TComponent);
begin
  inherited;

  RenderDebugToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  RenderDebugToggle.Caption := 'Render debug information';
  RenderDebugToggle.Checked := RenderDebug;
  RenderDebugToggle.OnClick := @ClickRenderDebug;

  ShadowVolumesRenderToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  ShadowVolumesRenderToggle.Caption := 'Render shadow volumes';
  ShadowVolumesRenderToggle.Checked := ShadowVolumesRender;
  ShadowVolumesRenderToggle.OnClick := @ClickShadowVolumesRender;

  DebugRenderForLevelScreenshotToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  DebugRenderForLevelScreenshotToggle.Caption := 'Render for level screenshot';
  DebugRenderForLevelScreenshotToggle.Checked := DebugRenderForLevelScreenshot;
  DebugRenderForLevelScreenshotToggle.OnClick := @ClickDebugRenderForLevelScreenshot;

  Add('Player debug menu ...', @ClickPlayerMenu);
  Add('Creatures debug menu ...', @ClickCreaturesMenu);
  Add('Items debug menu ...', @ClickItemsMenu);
  Add('Level debug menu ...', @ClickLevelsMenu);
  Add('Reload resources resource.xml files', @ClickReloadResources);
  Add('Reload resource animation ...', @ClickReloadResourceAnimation);
  Add(RenderDebugToggle);
  Add(ShadowVolumesRenderToggle);
  Add(DebugRenderForLevelScreenshotToggle);
  Add('Reload sounds/index.xml', @ClickReloadSounds);
  Add('Back to game', @ClickBack);
end;

procedure TDebugMenu.ClickPlayerMenu(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugPlayerMenu;
end;

procedure TDebugMenu.ClickCreaturesMenu(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugCreaturesMenu;
end;

procedure TDebugMenu.ClickItemsMenu(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugItemsMenu;
end;

procedure TDebugMenu.ClickLevelsMenu(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugLevelMenu;
end;

procedure TDebugMenu.ClickReloadResources(Sender: TObject);
begin
  Resources.LoadFromFiles(true);
end;

procedure TDebugMenu.ClickReloadResourceAnimation(Sender: TObject);
var
  Resource: T3DResource;
begin
  if ChooseResource(Resource, false) then
  begin
    if Resource.UsageCount = 0 then
      MessageOK(Window, Format('Resource "%s" is not used by anything, ' +
        'cannot reload',  [Resource.Name])) else
      Resource.RedoPrepare(SceneManager.PrepareParams);
  end;
end;

procedure TDebugMenu.ClickRenderDebug(Sender: TObject);
begin
  { toggle our RenderDebug variable }
  RenderDebug := not RenderDebug;
  { set CastleCreatures and CastleItems debugging }
  TCreature.RenderDebug := RenderDebug;
  TItemOnWorld.RenderDebug := RenderDebug;
  { update UI }
  RenderDebugToggle.Checked := RenderDebug;
end;

procedure TDebugMenu.ClickShadowVolumesRender(Sender: TObject);
begin
  ShadowVolumesRender := not ShadowVolumesRender;
  ShadowVolumesRenderToggle.Checked := ShadowVolumesRender;
  { apply to SceneManager.ShadowVolumesRender }
  if SceneManager <> nil then
    SceneManager.ShadowVolumesRender := ShadowVolumesRender;
end;

procedure TDebugMenu.ClickDebugRenderForLevelScreenshot(Sender: TObject);
begin
  DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
  DebugRenderForLevelScreenshotToggle.Checked := DebugRenderForLevelScreenshot;

  SceneManager.LevelProperties.ItemsRoot.Exists := not DebugRenderForLevelScreenshot;
end;

procedure TDebugMenu.ClickReloadSounds(Sender: TObject);
begin
  SoundEngine.ReloadSounds;
end;

procedure TDebugMenu.ClickBack(Sender: TObject);
begin
  TUIState.Pop(StateDebugMenu);
end;

{ TDebugPlayerMenu ----------------------------------------------------------- }

constructor TDebugPlayerMenu.Create(AOwner: TComponent);
begin
  inherited;

  { Note that Player is not created at this point.
    We will init Value of these sliders later. }
  RotationHorizontalSpeedSlider := TCastleFloatSlider.Create(Self);
  RotationHorizontalSpeedSlider.Min := 25;
  RotationHorizontalSpeedSlider.Max := 500;
  RotationHorizontalSpeedSlider.Value := 1;
  RotationHorizontalSpeedSlider.OnChange := @RotationHorizontalSpeedChanged;

  RotationVerticalSpeedSlider := TCastleFloatSlider.Create(Self);
  RotationVerticalSpeedSlider.Min := 25;
  RotationVerticalSpeedSlider.Max := 500;
  RotationVerticalSpeedSlider.Value := 1;
  RotationVerticalSpeedSlider.OnChange := @RotationVerticalSpeedChanged;

  PlayerSpeedSlider := TCastleFloatSlider.Create(Self);
  PlayerSpeedSlider.Min := 0.1;
  PlayerSpeedSlider.Max := 5;
  PlayerSpeedSlider.Value := 1;
  PlayerSpeedSlider.OnChange := @PlayerSpeedChanged;

  Add('Infinite Life', @ClickInfiniteLife);
  Add('Fly (indefinitely)', @ClickFly);
  Add('Set horizontal rotation speed', RotationHorizontalSpeedSlider);
  Add('Set vertical rotation speed', RotationVerticalSpeedSlider);
  Add('Set player speed', PlayerSpeedSlider);
  Add('Reload player.xml file', @ClickReloadPlayerXml);
  Add('Back', @ClickBack);
end;

procedure TDebugPlayerMenu.ClickInfiniteLife(Sender: TObject);
begin
  if Player.Dead then
    MessageOK(Window, 'No can do. You are dead.') else
  begin
    Player.MaxLife := 10000;
    Player.Life := Player.MaxLife;
    TUIState.Pop(StateDebugMenu);
  end;
end;

procedure TDebugPlayerMenu.ClickFly(Sender: TObject);
begin
  Player.Flying := true;
end;

procedure TDebugPlayerMenu.ClickReloadPlayerXml(Sender: TObject);
begin
  Player.LoadFromFile;
end;

procedure TDebugPlayerMenu.ClickBack(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugMenu;
end;

procedure TDebugPlayerMenu.RotationHorizontalSpeedChanged(Sender: TObject);
begin
  Player.WalkNavigation.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
end;

procedure TDebugPlayerMenu.RotationVerticalSpeedChanged(Sender: TObject);
begin
  Player.WalkNavigation.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
end;

procedure TDebugPlayerMenu.PlayerSpeedChanged(Sender: TObject);
begin
  Player.WalkNavigation.MoveSpeed := PlayerSpeedSlider.Value;
end;

{ TDebugCreaturesMenu -------------------------------------------------------- }

constructor TDebugCreaturesMenu.Create(AOwner: TComponent);
begin
  inherited;

  DebugTimeStopForCreaturesToggle := TCastleOnScreenMenuItemToggle.Create(Self);
  DebugTimeStopForCreaturesToggle.Caption := 'Time stop for creatures';
  DebugTimeStopForCreaturesToggle.Checked := DebugTimeStopForCreatures;
  DebugTimeStopForCreaturesToggle.OnClick := @ClickDebugTimeStopForCreatures;

  Add('Kill all creatures', @ClickKillAll);
  Add('Kill all non-still creatures', @ClickKillAllNonStill);
  Add('Add creature to level before player', @ClickAddCreature);
  Add(DebugTimeStopForCreaturesToggle);
  Add('Back', @ClickBack);
end;

procedure TDebugCreaturesMenu.KillAllCore(const IncludeStill: boolean);
var
  I: Integer;
  C: TCreature;
  CreaturesRoot: TCastleTransform;
begin
  CreaturesRoot := SceneManager.LevelProperties.CreaturesRoot;
  for I := 0 to CreaturesRoot.Count - 1 do
    if CreaturesRoot[I] is TCreature then
    begin
      C := TCreature(CreaturesRoot[I]);
      if (C.Life > 0) and (IncludeStill or (not (C is TStillCreature))) then
      begin
        C.SoundDieEnabled := false;
        C.Hurt(1000 * 1000, TVector3.Zero, 0, nil);
      end;
    end;
end;

procedure TDebugCreaturesMenu.ClickKillAll(Sender: TObject);
begin
  KillAllCore(true);
end;

procedure TDebugCreaturesMenu.ClickKillAllNonStill(Sender: TObject);
begin
  KillAllCore(false);
end;

procedure TDebugCreaturesMenu.ClickAddCreature(Sender: TObject);
const
  DirectionAttenuation = 10.0;
var
  Resource: T3DResource;
begin
  if ChooseResource(Resource, true) then
  begin
    (Resource as TCreatureResource).CreateCreature(SceneManager.LevelProperties,
      Player.Translation + Player.Direction * DirectionAttenuation,
      Player.Direction);

    TUIState.Pop(StateDebugMenu);
  end;
end;

procedure TDebugCreaturesMenu.ClickDebugTimeStopForCreatures(Sender: TObject);
begin
  DebugTimeStopForCreatures := not DebugTimeStopForCreatures;
  DebugTimeStopForCreaturesToggle.Checked := DebugTimeStopForCreatures;
end;

procedure TDebugCreaturesMenu.ClickBack(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugMenu;
end;

{ TDebugItemsMenu ------------------------------------------------------------ }

constructor TDebugItemsMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Give me 20 instances of every possible item', @ClickGiveAll);
  Add('Back', @ClickBack);
end;

procedure TDebugItemsMenu.ClickGiveAll(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Resources.Count - 1 do
    if Resources[I] is TItemResource then
      Player.PickItem(TItemResource(Resources[I]).CreateItem(20));
  TUIState.Pop(StateDebugMenu);
end;

procedure TDebugItemsMenu.ClickBack(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugMenu;
end;

{ TDebugLevelMenu -------------------------------------------------------- }

constructor TDebugLevelMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Change to level', @ClickChangeLevel);
  Add('Restart current level (preserving camera)', @ClickRestart);
  Add('Back', @ClickBack);
end;

procedure TDebugLevelMenu.ClickChangeLevel(Sender: TObject);
var
  S: TStringList;
  I, Index: Integer;
begin
  S := TStringList.Create;
  try
    for I := 0 to Levels.Count - 1 do
    begin
      S.Append(Format('Level %d "%s"',
        [ Levels[I].Number, Levels[I].Title ]));
    end;
    S.Append('Cancel');

    Index := ChooseByMenu(S);

    if Index <> Levels.Count then
    begin
      LevelFinished(Levels[Index].Name);
      { Flush LevelFinished now, to give new items when new level is loaded.
        Otherwise, some sounds (like equipping the sword, if player gets
        his first weapon) could be done before loading level progress,
        which sounds awkward for player. }
      LevelFinishedFlush;
      SceneManager.Logic.PrepareNewPlayer(Player);
      TUIState.Pop(StateDebugMenu);
    end;
  finally S.Free end;
end;

procedure TDebugLevelMenu.ClickRestart(Sender: TObject);
var
  Pos, Dir, Up: TVector3;
begin
  Pos := SceneManager.Camera.Position;
  Dir := SceneManager.Camera.Direction;
  Up := SceneManager.Camera.Up;

  LevelFinished(SceneManager.Info.Name);
  LevelFinishedFlush;

  SceneManager.Camera.Position := Pos;
  SceneManager.Camera.Direction := Dir;
  SceneManager.Camera.Up := Up;

  TUIState.Pop(StateDebugMenu);
end;

procedure TDebugLevelMenu.ClickBack(Sender: TObject);
begin
  StateDebugMenu.CurrentMenu := DebugMenu;
end;

{ TStateDebugMenu ------------------------------------------------------------ }

constructor TStateDebugMenu.Create(AOwner: TComponent);
begin
  inherited;
  DebugMenu := TDebugMenu.Create(Application);
  DebugPlayerMenu := TDebugPlayerMenu.Create(Application);
  DebugCreaturesMenu := TDebugCreaturesMenu.Create(Application);
  DebugLevelMenu := TDebugLevelMenu.Create(Application);
  DebugItemsMenu := TDebugItemsMenu.Create(Application);
end;

procedure TStateDebugMenu.Start;
begin
  inherited;
  DebugPlayerMenu.RotationHorizontalSpeedSlider.Value :=
    Player.WalkNavigation.RotationHorizontalSpeed;
  DebugPlayerMenu.RotationVerticalSpeedSlider.Value :=
    Player.WalkNavigation.RotationVerticalSpeed;
  DebugPlayerMenu.PlayerSpeedSlider.Value := Player.WalkNavigation.MoveSpeed;

  OldThemeWindow := Theme.ImagesPersistent[tiWindow].Url;
  { Otherwise CastleMessages don't look good,
    as mesage text would be mixed with the menu text underneath. }
  Theme.ImagesPersistent[tiWindow].Url := 'castle-data:/theme/WindowDark.png';

  CurrentMenu := DebugMenu;
end;

procedure TStateDebugMenu.Stop;
begin
  CurrentMenu := nil;
  Theme.ImagesPersistent[tiWindow].Url := OldThemeWindow;
  inherited;
end;

function TStateDebugMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(CharEscape) then
  begin
    TUIState.Pop(StateDebugMenu);
    Result := true;
  end;
end;

end.
