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

uses CastleUIControls;

procedure ShowDebugMenu(AControlsUnder: TUIControlList);

implementation

uses SysUtils, Classes, CastleControlsImages, CastleImages,
  CastleUtils, CastleStringUtils, CastleWindowModes,
  CastleGLUtils, CastleMessages, GameWindow, Castle3D,
  CastleVectors, CastleWindow, GamePlay, GameGeneralMenu,
  CastleInputs, CastleCreatures, GameChooseMenu,
  CastleItems, CastleOnScreenMenu, CastleRays, GameVideoOptions, CastleSoundEngine,
  X3DNodes, CastleClassUtils, CastleGameNotifications,
  CastleLevels, CastleKeysMouse, CastleResources, CastleControls;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TDebugMenu = class(TCastleGameMenu)
  public
    RenderDebug3DArgument: TMenuBooleanArgument;
    RenderDebugCaptionsArgument: TMenuBooleanArgument;
    ShadowVolumesRenderArgument: TMenuBooleanArgument;
    DebugRenderForLevelScreenshotArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugPlayerMenu = class(TCastleGameMenu)
  public
    RotationHorizontalSpeedSlider: TMenuFloatSlider;
    RotationVerticalSpeedSlider: TMenuFloatSlider;
    PlayerSpeedSlider: TMenuFloatSlider;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TDebugItemsMenu = class(TCastleGameMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugCreaturesMenu = class(TCastleGameMenu)
  public
    DebugTimeStopForCreaturesArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugLevelMenu = class(TCastleGameMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleGameMenu descendants implementation) }

var
  UserQuit: boolean;
  ControlsUnder: TUIControlList;
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
    ResultIndex := ChooseByMenu(ControlsUnder, S);
    Result := ResultIndex <> S.Count - 1;
    if Result then
      Resource := S.Objects[ResultIndex] as T3DResource;
  finally S.Free end;
end;

{ TDebugMenu ------------------------------------------------------------ }

constructor TDebugMenu.Create(AOwner: TComponent);
begin
  inherited;

  RenderDebug3DArgument := TMenuBooleanArgument.Create(RenderDebug3D);
  RenderDebugCaptionsArgument := TMenuBooleanArgument.Create(RenderDebugCaptions);
  ShadowVolumesRenderArgument := TMenuBooleanArgument.Create(ShadowVolumesRender);
  DebugRenderForLevelScreenshotArgument := TMenuBooleanArgument.Create(
    DebugRenderForLevelScreenshot);

  Items.Add('Player debug menu ...');
  Items.Add('Creatures debug menu ...');
  Items.Add('Items debug menu ...');
  Items.Add('Level debug menu ...');
  Items.Add('Reload resources resource.xml files');
  Items.Add('Reload resource animation ...');
  Items.AddObject('Render debug 3D information', RenderDebug3DArgument);
  Items.AddObject('Render debug captions', RenderDebugCaptionsArgument);
  Items.AddObject('Render shadow volumes', ShadowVolumesRenderArgument);
  Items.AddObject('Render for level screenshot', DebugRenderForLevelScreenshotArgument);
  Items.Add('Reload sounds/index.xml');
  Items.Add('Back to game');
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
         RenderDebug3DArgument.Value := RenderDebug3D;
       end;
    7: begin
         RenderDebugCaptions := not RenderDebugCaptions;
         RenderDebugCaptionsArgument.Value := RenderDebugCaptions;
       end;
    8: begin
         ShadowVolumesRender := not ShadowVolumesRender;
         ShadowVolumesRenderArgument.Value := ShadowVolumesRender;
       end;
    9: begin
         DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
         DebugRenderForLevelScreenshotArgument.Value :=
           DebugRenderForLevelScreenshot;
       end;
    10:SoundEngine.ReloadSounds;
    11:UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TDebugPlayerMenu ----------------------------------------------------------- }

constructor TDebugPlayerMenu.Create(AOwner: TComponent);
begin
  inherited;

  { Note that Player is not created at this point.
    We will init Value of these sliders later. }
  RotationHorizontalSpeedSlider := TMenuFloatSlider.Create(25, 500, 1);
  RotationVerticalSpeedSlider := TMenuFloatSlider.Create(25, 500, 1);
  PlayerSpeedSlider := TMenuFloatSlider.Create(0.1, 5, 1);

  Items.Add('Infinite Life');
  Items.Add('Fly (indefinitely)');
  Items.AddObject('Set horizontal rotation speed', RotationHorizontalSpeedSlider);
  Items.AddObject('Set vertical rotation speed', RotationVerticalSpeedSlider);
  Items.AddObject('Set player speed', PlayerSpeedSlider);
  Items.Add('Reload player.xml file');
  Items.Add('Back');
end;

procedure TDebugPlayerMenu.Click;

  procedure PlayerSetMaxLife;
  begin
    if Player.Dead then
      MessageOK(Window, 'No can do. You are dead.') else
    begin
      Player.MaxLife := 10000;
      Player.Life := Player.MaxLife;
      UserQuit := true;
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

procedure TDebugPlayerMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    2: Player.Camera.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
    3: Player.Camera.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
    4: Player.Camera.MoveSpeed := PlayerSpeedSlider.Value;
  end;
end;

{ TDebugCreaturesMenu -------------------------------------------------------- }

constructor TDebugCreaturesMenu.Create(AOwner: TComponent);
begin
  inherited;

  DebugTimeStopForCreaturesArgument := TMenuBooleanArgument.Create(
    DebugTimeStopForCreatures);

  Items.Add('Kill all creatures');
  Items.Add('Kill all non-still creatures');
  Items.Add('Add creature to level before player');
  Items.AddObject('Time stop for creatures', DebugTimeStopForCreaturesArgument);
  Items.Add('Back');
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

      UserQuit := true;
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
         DebugTimeStopForCreaturesArgument.Value := DebugTimeStopForCreatures;
       end;
    4: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ TDebugLevelMenu -------------------------------------------------------- }

constructor TDebugLevelMenu.Create(AOwner: TComponent);
begin
  inherited;

  Items.Add('Change to level');
  Items.Add('Restart current level (preserving camera)');
  Items.Add('Back');
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

      Index := ChooseByMenu(ControlsUnder, S);

      if Index <> Levels.Count then
      begin
        LevelFinished(Levels[Index].Name);
        { Flush LevelFinished now, to give new items when new level is loaded.
          Otherwise, some sounds (like equipping the sword, if player gets
          his first weapon) could be done before loading level progress,
          which sounds awkward for player. }
        LevelFinishedFlush;
        SceneManager.Logic.PrepareNewPlayer(Player);
        UserQuit := true;
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

    UserQuit := true;
  end;

begin
  inherited;

  case CurrentItem of
    0: ChangeToLevel;
    1: RestartLevel;
    2: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

procedure TDebugLevelMenu.AccessoryValueChanged;
begin
  inherited;
end;

{ TDebugItemsMenu ------------------------------------------------------------ }

constructor TDebugItemsMenu.Create(AOwner: TComponent);
begin
  inherited;

  Items.Add('Give me 20 instances of every possible item');
  Items.Add('Back');
end;

procedure TDebugItemsMenu.Click;

  procedure GiveItems;
  var
    I: Integer;
  begin
    for I := 0 to Resources.Count - 1 do
      if Resources[I] is TItemResource then
        Player.PickItem(TItemResource(Resources[I]).CreateItem(20));
    UserQuit := true;
  end;

begin
  inherited;

  case CurrentItem of
    0: GiveItems;
    1: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ global things -------------------------------------------------------------- }

{$I gamemenucallbacks.inc}

procedure ShowDebugMenu(AControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
  OldThemeWindow: TCastleImage;
begin
  ControlsUnder := AControlsUnder;

  DebugPlayerMenu.RotationHorizontalSpeedSlider.Value :=
    Player.Camera.RotationHorizontalSpeed;
  DebugPlayerMenu.RotationVerticalSpeedSlider.Value :=
    Player.Camera.RotationVerticalSpeed;
  DebugPlayerMenu.PlayerSpeedSlider.Value := Player.Camera.MoveSpeed;

  OldThemeWindow := Theme.Images[tiWindow];
  SavedMode := TGLMode.CreateReset(Window, nil, Window.OnResize, @CloseQuery);
  try
    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    Theme.Images[tiWindow] := WindowDark;

    Window.OnPress := @Press;
    Window.RenderStyle := rs3D;

    SetCurrentMenu(CurrentMenu, DebugMenu);

    Window.Controls.Add(GlobalCatchInput);
    Window.Controls.AddList(ControlsUnder);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true, true);
    until GameEnded or UserQuit;
  finally
    FreeAndNil(SavedMode);
    Theme.Images[tiWindow] := OldThemeWindow;
  end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure ContextOpen;
begin
  { Although base TCastleOnScreenMenu doesn't require OpenGL context at constructor,
    our descendants initialize some arguments that require font initialized
    that requires font display lists created. That's why code below is in
    OnOpen callback, not unit's initialization. }
  DebugMenu := TDebugMenu.Create(Application);
  DebugPlayerMenu := TDebugPlayerMenu.Create(Application);
  DebugCreaturesMenu := TDebugCreaturesMenu.Create(Application);
  DebugLevelMenu := TDebugLevelMenu.Create(Application);
  DebugItemsMenu := TDebugItemsMenu.Create(Application);
end;

initialization
  OnGLContextOpen.Add(@ContextOpen);
end.
