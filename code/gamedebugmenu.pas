{
  Copyright 2006-2017 Michalis Kamburelis.

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
    OldThemeWindow: TCastleImage;
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
  CastleControlsImages, CastleUtils, CastleStringUtils,
  CastleGLUtils, CastleMessages, GameWindow, Castle3D,
  CastleVectors, CastleWindow, GamePlay,
  CastleInputs, CastleCreatures, GameChooseMenu,
  CastleItems, CastleOnScreenMenu, CastleRays, GameVideoOptions, CastleSoundEngine,
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
    procedure ClickRenderDebug3D(Sender: TObject);
    procedure ClickRenderDebugCaptions(Sender: TObject);
    procedure ClickShadowVolumesRender(Sender: TObject);
    procedure ClickDebugRenderForLevelScreenshot(Sender: TObject);
    procedure ClickReloadSounds(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    RenderDebug3DToggle: TCastleMenuToggle;
    RenderDebugCaptionsToggle: TCastleMenuToggle;
    ShadowVolumesRenderToggle: TCastleMenuToggle;
    DebugRenderForLevelScreenshotToggle: TCastleMenuToggle;
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
    DebugTimeStopForCreaturesToggle: TCastleMenuToggle;
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

  RenderDebug3DToggle := TCastleMenuToggle.Create(Self);
  RenderDebug3DToggle.Pressed := RenderDebug3D;
  RenderDebug3DToggle.OnClick := @ClickRenderDebug3D;

  RenderDebugCaptionsToggle := TCastleMenuToggle.Create(Self);
  RenderDebugCaptionsToggle.Pressed := RenderDebugCaptions;
  RenderDebugCaptionsToggle.OnClick := @ClickRenderDebugCaptions;

  ShadowVolumesRenderToggle := TCastleMenuToggle.Create(Self);
  ShadowVolumesRenderToggle.Pressed := ShadowVolumesRender;
  ShadowVolumesRenderToggle.OnClick := @ClickShadowVolumesRender;

  DebugRenderForLevelScreenshotToggle := TCastleMenuToggle.Create(Self);
  DebugRenderForLevelScreenshotToggle.Pressed := DebugRenderForLevelScreenshot;
  DebugRenderForLevelScreenshotToggle.OnClick := @ClickDebugRenderForLevelScreenshot;

  Add('Player debug menu ...', @ClickPlayerMenu);
  Add('Creatures debug menu ...', @ClickCreaturesMenu);
  Add('Items debug menu ...', @ClickItemsMenu);
  Add('Level debug menu ...', @ClickLevelsMenu);
  Add('Reload resources resource.xml files', @ClickReloadResources);
  Add('Reload resource animation ...', @ClickReloadResourceAnimation);
  Add('Render debug 3D information', RenderDebug3DToggle);
  Add('Render debug captions', RenderDebugCaptionsToggle);
  Add('Render shadow volumes', ShadowVolumesRenderToggle);
  Add('Render for level screenshot', DebugRenderForLevelScreenshotToggle);
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
      Resource.RedoPrepare(SceneManager.BaseLights);
  end;
end;

procedure TDebugMenu.ClickRenderDebug3D(Sender: TObject);
begin
  RenderDebug3D := not RenderDebug3D;
  RenderDebug3DToggle.Pressed := RenderDebug3D;
end;

procedure TDebugMenu.ClickRenderDebugCaptions(Sender: TObject);
begin
  RenderDebugCaptions := not RenderDebugCaptions;
  RenderDebugCaptionsToggle.Pressed := RenderDebugCaptions;
end;

procedure TDebugMenu.ClickShadowVolumesRender(Sender: TObject);
begin
  ShadowVolumesRender := not ShadowVolumesRender;
  ShadowVolumesRenderToggle.Pressed := ShadowVolumesRender;
  { apply to SceneManager.ShadowVolumesRender }
  if SceneManager <> nil then
    SceneManager.ShadowVolumesRender := ShadowVolumesRender;
end;

procedure TDebugMenu.ClickDebugRenderForLevelScreenshot(Sender: TObject);
begin
  DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
  DebugRenderForLevelScreenshotToggle.Pressed := DebugRenderForLevelScreenshot;
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
  Player.Camera.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
end;

procedure TDebugPlayerMenu.RotationVerticalSpeedChanged(Sender: TObject);
begin
  Player.Camera.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
end;

procedure TDebugPlayerMenu.PlayerSpeedChanged(Sender: TObject);
begin
  Player.Camera.MoveSpeed := PlayerSpeedSlider.Value;
end;

{ TDebugCreaturesMenu -------------------------------------------------------- }

constructor TDebugCreaturesMenu.Create(AOwner: TComponent);
begin
  inherited;

  DebugTimeStopForCreaturesToggle := TCastleMenuToggle.Create(Self);
  DebugTimeStopForCreaturesToggle.Pressed := DebugTimeStopForCreatures;
  DebugTimeStopForCreaturesToggle.OnClick := @ClickDebugTimeStopForCreatures;

  Add('Kill all creatures', @ClickKillAll);
  Add('Kill all non-still creatures', @ClickKillAllNonStill);
  Add('Add creature to level before player', @ClickAddCreature);
  Add('Time stop for creatures', DebugTimeStopForCreaturesToggle);
  Add('Back', @ClickBack);
end;

procedure TDebugCreaturesMenu.KillAllCore(const IncludeStill: boolean);
var
  I: Integer;
  C: TCreature;
begin
  for I := 0 to SceneManager.Items.Count - 1 do
    if SceneManager.Items[I] is TCreature then
    begin
      C := TCreature(SceneManager.Items[I]);
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
    (Resource as TCreatureResource).CreateCreature(SceneManager.Items,
      Player.Translation + Player.Direction * DirectionAttenuation,
      Player.Direction);

    TUIState.Pop(StateDebugMenu);
  end;
end;

procedure TDebugCreaturesMenu.ClickDebugTimeStopForCreatures(Sender: TObject);
begin
  DebugTimeStopForCreatures := not DebugTimeStopForCreatures;
  DebugTimeStopForCreaturesToggle.Pressed := DebugTimeStopForCreatures;
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
  Pos := Player.Camera.Position;
  Dir := Player.Camera.Direction;
  Up := Player.Camera.Up;

  LevelFinished(SceneManager.Info.Name);
  LevelFinishedFlush;

  { Change Player.Camera, they will be automatically set also for Player
    properties by TCastleSceneManager.CameraVisibleChange.
    TODO: maybe different one day? }

  Player.Camera.Position := Pos;
  Player.Camera.Direction := Dir;
  Player.Camera.Up := Up;

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
    Player.Camera.RotationHorizontalSpeed;
  DebugPlayerMenu.RotationVerticalSpeedSlider.Value :=
    Player.Camera.RotationVerticalSpeed;
  DebugPlayerMenu.PlayerSpeedSlider.Value := Player.Camera.MoveSpeed;

  OldThemeWindow := Theme.Images[tiWindow];
  { Otherwise messages don't look good, because the text is mixed
    with the menu text. }
  Theme.Images[tiWindow] := WindowDark;

  CurrentMenu := DebugMenu;
end;

procedure TStateDebugMenu.Stop;
begin
  CurrentMenu := nil;
  Theme.Images[tiWindow] := OldThemeWindow;
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
