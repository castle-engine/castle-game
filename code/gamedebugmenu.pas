{
  Copyright 2006-2014 Michalis Kamburelis.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ }
unit GameDebugMenu;

interface

uses Classes,
  CastleUIState, CastleUIControls, CastleKeysMouse, CastleImages;

type
  TStateDebugMenu = class(TUIState)
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
  CastleControlsImages, CastleUtils, CastleStringUtils, CastleWindowModes,
  CastleGLUtils, CastleMessages, GameWindow, Castle3D,
  CastleVectors, CastleWindow, GamePlay, GameGeneralMenu,
  CastleInputs, CastleCreatures, GameChooseMenu,
  CastleItems, CastleOnScreenMenu, CastleRays, GameVideoOptions, CastleSoundEngine,
  X3DNodes, CastleClassUtils, CastleGameNotifications,
  CastleLevels, CastleResources, CastleControls, CastleApplicationProperties;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TDebugMenu = class(TCastleGameMenu)
  public
    RenderDebug3DToggle: TCastleMenuToggle;
    RenderDebugCaptionsToggle: TCastleMenuToggle;
    ShadowVolumesRenderToggle: TCastleMenuToggle;
    DebugRenderForLevelScreenshotToggle: TCastleMenuToggle;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugPlayerMenu = class(TCastleGameMenu)
  private
    procedure RotationHorizontalSpeedChanged(Sender: TObject);
    procedure RotationVerticalSpeedChanged(Sender: TObject);
    procedure PlayerSpeedChanged(Sender: TObject);
  public
    RotationHorizontalSpeedSlider: TCastleFloatSlider;
    RotationVerticalSpeedSlider: TCastleFloatSlider;
    PlayerSpeedSlider: TCastleFloatSlider;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugItemsMenu = class(TCastleGameMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugCreaturesMenu = class(TCastleGameMenu)
  public
    DebugTimeStopForCreaturesToggle: TCastleMenuToggle;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugLevelMenu = class(TCastleGameMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  CurrentMenu: TCastleGameMenu;

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

  RenderDebugCaptionsToggle := TCastleMenuToggle.Create(Self);
  RenderDebugCaptionsToggle.Pressed := RenderDebugCaptions;

  ShadowVolumesRenderToggle := TCastleMenuToggle.Create(Self);
  ShadowVolumesRenderToggle.Pressed := ShadowVolumesRender;

  DebugRenderForLevelScreenshotToggle := TCastleMenuToggle.Create(Self);
  DebugRenderForLevelScreenshotToggle.Pressed := DebugRenderForLevelScreenshot;

  Add('Player debug menu ...');
  Add('Creatures debug menu ...');
  Add('Items debug menu ...');
  Add('Level debug menu ...');
  Add('Reload resources resource.xml files');
  Add('Reload resource animation ...');
  Add('Render debug 3D information', RenderDebug3DToggle);
  Add('Render debug captions', RenderDebugCaptionsToggle);
  Add('Render shadow volumes', ShadowVolumesRenderToggle);
  Add('Render for level screenshot', DebugRenderForLevelScreenshotToggle);
  Add('Reload sounds/index.xml');
  Add('Back to game');
end;

procedure TDebugMenu.Click;

  procedure ReloadResource;
  var
    Resource: T3DResource;
  begin
    if ChooseResource(Resource, false) then
    begin
      if Resource.UsageCount = 0 then
        MessageOK(Window, Format('Resource "%s" is not used by anything, ' +
          'cannot reload',  [Resource.Name])) else
        Resource.RedoPrepare(SceneManager.BaseLights, SceneManager.GravityUp);
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: SetCurrentMenu(CurrentMenu, DebugPlayerMenu);
    1: SetCurrentMenu(CurrentMenu, DebugCreaturesMenu);
    2: SetCurrentMenu(CurrentMenu, DebugItemsMenu);
    3: SetCurrentMenu(CurrentMenu, DebugLevelMenu);
    4: Resources.LoadFromFiles(true);
    5: ReloadResource;
    6: begin
         RenderDebug3D := not RenderDebug3D;
         RenderDebug3DToggle.Pressed := RenderDebug3D;
       end;
    7: begin
         RenderDebugCaptions := not RenderDebugCaptions;
         RenderDebugCaptionsToggle.Pressed := RenderDebugCaptions;
       end;
    8: begin
         ShadowVolumesRender := not ShadowVolumesRender;
         ShadowVolumesRenderToggle.Pressed := ShadowVolumesRender;
       end;
    9: begin
         DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
         DebugRenderForLevelScreenshotToggle.Pressed :=
           DebugRenderForLevelScreenshot;
       end;
    10:SoundEngine.ReloadSounds;
    11:TUIState.Pop(StateDebugMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
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

  Add('Infinite Life');
  Add('Fly (indefinitely)');
  Add('Set horizontal rotation speed', RotationHorizontalSpeedSlider);
  Add('Set vertical rotation speed', RotationVerticalSpeedSlider);
  Add('Set player speed', PlayerSpeedSlider);
  Add('Reload player.xml file');
  Add('Back');
end;

procedure TDebugPlayerMenu.Click;

  procedure PlayerSetMaxLife;
  begin
    if Player.Dead then
      MessageOK(Window, 'No can do. You are dead.') else
    begin
      Player.MaxLife := 10000;
      Player.Life := Player.MaxLife;
      TUIState.Pop(StateDebugMenu);
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: PlayerSetMaxLife;
    1: Player.Flying := true;
    2: ;
    3: ;
    4: ;
    5: Player.LoadFromFile;
    6: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
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

  Add('Kill all creatures');
  Add('Kill all non-still creatures');
  Add('Add creature to level before player');
  Add('Time stop for creatures', DebugTimeStopForCreaturesToggle);
  Add('Back');
end;

procedure TDebugCreaturesMenu.Click;

  procedure KillAll(const IncludeStill: boolean);
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
          C.Hurt(1000 * 1000, ZeroVector3Single, 0, nil);
        end;
      end;
  end;

  procedure AddLevelCreature(DirectionAttenuation: Single);
  var
    Resource: T3DResource;
  begin
    if ChooseResource(Resource, true) then
    begin
      (Resource as TCreatureResource).CreateCreature(SceneManager.Items,
        Player.Position + Player.Direction * DirectionAttenuation,
        Player.Direction);

      TUIState.Pop(StateDebugMenu);
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: KillAll(true);
    1: KillAll(false);
    2: AddLevelCreature(10);
    3: begin
         DebugTimeStopForCreatures := not DebugTimeStopForCreatures;
         DebugTimeStopForCreaturesToggle.Pressed := DebugTimeStopForCreatures;
       end;
    4: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ TDebugLevelMenu -------------------------------------------------------- }

constructor TDebugLevelMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Change to level');
  Add('Restart current level (preserving camera)');
  Add('Back');
end;

procedure TDebugLevelMenu.Click;

  procedure ChangeToLevel;
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

  procedure RestartLevel;
  var
    Pos, Dir, Up: TVector3Single;
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

begin
  inherited;

  case CurrentItem of
    0: ChangeToLevel;
    1: RestartLevel;
    2: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ TDebugItemsMenu ------------------------------------------------------------ }

constructor TDebugItemsMenu.Create(AOwner: TComponent);
begin
  inherited;

  Add('Give me 20 instances of every possible item');
  Add('Back');
end;

procedure TDebugItemsMenu.Click;

  procedure GiveItems;
  var
    I: Integer;
  begin
    for I := 0 to Resources.Count - 1 do
      if Resources[I] is TItemResource then
        Player.PickItem(TItemResource(Resources[I]).CreateItem(20));
    TUIState.Pop(StateDebugMenu);
  end;

begin
  inherited;

  case CurrentItem of
    0: GiveItems;
    1: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
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

  SetCurrentMenu(CurrentMenu, DebugMenu);
end;

procedure TStateDebugMenu.Stop;
begin
  SetCurrentMenu(CurrentMenu, nil);
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
