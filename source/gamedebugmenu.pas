{
  Copyright 2006-2012 Michalis Kamburelis.

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

uses UIControls;

procedure ShowDebugMenu(AControlsUnder: TUIControlList);

implementation

uses SysUtils, Classes, CastleUtils, CastleStringUtils, WindowModes,
  GL, GLU, CastleGLUtils, CastleMessages, CastleGameWindow,
  VectorMath, CastleWindow, GamePlay, GameGeneralMenu,
  GameControlsMenu, GameInputs, CastleCreatures, GameChooseMenu,
  GameItems, OnScreenMenu, RaysWindow, GameVideoOptions,
  GameSound, X3DNodes, CastleClassUtils, CastleGameNotifications,
  GameLevel, KeysMouse, CastleResources, CastleGameVideoOptions;

{ TCastleGameMenu descendants interface ------------------------------------------ }

type
  TViewAngleSlider = class(TMenuFloatSlider)
  public
    constructor Create;
    function ValueToStr(const AValue: Single): string; override;
  end;

  TDebugMenu = class(TCastleGameMenu)
  public
    RenderBoundingBoxesArgument: TMenuBooleanArgument;
    RenderDebugCaptionsArgument: TMenuBooleanArgument;
    DebugRenderShadowVolumeArgument: TMenuBooleanArgument;
    DebugRenderForLevelScreenshotArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugPlayerMenu = class(TCastleGameMenu)
  public
    ViewAngleSlider: TViewAngleSlider;
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

  TEditLevelLightsMenu = class(TCastleGameMenu)
  public
    AmbientColorSlider: array[0..2] of TMenuFloatSlider;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TEditOneLightMenu = class(TCastleGameMenu)
  public
    Light: TAbstractLightNode;
    RedColorSlider: TMenuFloatSlider;
    GreenColorSlider: TMenuFloatSlider;
    BlueColorSlider: TMenuFloatSlider;
    IntensitySlider: TMenuFloatSlider;
    AmbientIntensitySlider: TMenuFloatSlider;
    OnArgument: TMenuBooleanArgument;
    ShadowsArgument: TMenuBooleanArgument;
    ShadowsMainArgument: TMenuBooleanArgument;
    PositionSlider: array [0..2] of TMenuFloatSlider;
    constructor Create(AOwner: TComponent; ALight: TAbstractLightNode); reintroduce;
    procedure Click; override;
    procedure AccessoryValueChanged; override;

    function GetLightLocation: TVector3Single;
    procedure SetLightLocation(const Value: TVector3Single);
    property LightLocation: TVector3Single
      read GetLightLocation write SetLightLocation;
  end;

  TEditHeadlightMenu = class(TCastleGameMenu)
  public
    Headlight: TAbstractLightNode;
    AmbientIntensitySlider: TMenuFloatSlider;
    ColorSlider: array[0..2] of TMenuFloatSlider;
    IntensitySlider: TMenuFloatSlider;
    SpotArgument: TMenuBooleanArgument;
    constructor Create(AOwner: TComponent; AHeadlight: TAbstractLightNode); reintroduce;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

{ TViewAngleSlider ----------------------------------------------------------- }

constructor TViewAngleSlider.Create;
begin
  inherited Create(10, 170, ViewAngleDegX);
end;

function TViewAngleSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('horiz %f, vert %f', [AValue,
    AdjustViewAngleDegToAspectRatio(AValue, Window.Height / Window.Width)]);
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
  EditLevelLightsMenu: TEditLevelLightsMenu;
  EditOneLightMenu: TEditOneLightMenu;
  EditHeadlightMenu: TEditHeadlightMenu;

{ utility -------------------------------------------------------------------- }

function ChooseResource(out Resource: T3DResource;
  const OnlyCreatures: boolean): boolean;
var
  S: TStringList;
  I, ResultIndex: Integer;
begin
  S := TStringList.Create;
  try
    for I := 0 to AllResources.Count - 1 do
      if (not OnlyCreatures) or (AllResources[I] is TCreatureKind) then
        S.AddObject(Format('Resource %s (%d users)',
          [AllResources[I].Id, AllResources[I].UsageCount]), AllResources[I]);
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

  RenderBoundingBoxesArgument := TMenuBooleanArgument.Create(RenderBoundingBoxes);
  RenderDebugCaptionsArgument := TMenuBooleanArgument.Create(RenderDebugCaptions);
  DebugRenderShadowVolumeArgument := TMenuBooleanArgument.Create(DebugRenderShadowVolume);
  DebugRenderForLevelScreenshotArgument := TMenuBooleanArgument.Create(
    DebugRenderForLevelScreenshot);

  Items.Add('Player debug menu ...');
  Items.Add('Creatures debug menu ...');
  Items.Add('Items debug menu ...');
  Items.Add('Level debug menu ...');
  Items.Add('Reload resources index.xml files');
  Items.Add('Reload resource animation ...');
  Items.AddObject('Render bounding boxes', RenderBoundingBoxesArgument);
  Items.AddObject('Render debug captions', RenderDebugCaptionsArgument);
  Items.AddObject('Render shadow volumes', DebugRenderShadowVolumeArgument);
  Items.AddObject('Render for level screenshot', DebugRenderForLevelScreenshotArgument);
  Items.Add('Reload sounds/index.xml');
  Items.Add('Edit lights ...');
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
          'cannot reload',  [Resource.Id])) else
        Resource.RedoPrepare(SceneManager.BaseLights);
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: SetCurrentMenu(CurrentMenu, DebugPlayerMenu);
    1: SetCurrentMenu(CurrentMenu, DebugCreaturesMenu);
    2: SetCurrentMenu(CurrentMenu, DebugItemsMenu);
    3: SetCurrentMenu(CurrentMenu, DebugLevelMenu);
    4: AllResources.LoadFromFiles(true);
    5: ReloadResource;
    6: begin
         RenderBoundingBoxes := not RenderBoundingBoxes;
         RenderBoundingBoxesArgument.Value := RenderBoundingBoxes;
       end;
    7: begin
         RenderDebugCaptions := not RenderDebugCaptions;
         RenderDebugCaptionsArgument.Value := RenderDebugCaptions;
       end;
    8: begin
         DebugRenderShadowVolume := not DebugRenderShadowVolume;
         DebugRenderShadowVolumeArgument.Value := DebugRenderShadowVolume;
       end;
    9: begin
         DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
         DebugRenderForLevelScreenshotArgument.Value :=
           DebugRenderForLevelScreenshot;
       end;
    10:SoundEngine.ReadSounds;
    11:begin
         FreeAndNil(EditLevelLightsMenu);
         EditLevelLightsMenu := TEditLevelLightsMenu.Create(Application);
         SetCurrentMenu(CurrentMenu, EditLevelLightsMenu);
       end;
    12:UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TDebugPlayerMenu ----------------------------------------------------------- }

constructor TDebugPlayerMenu.Create(AOwner: TComponent);
begin
  inherited;

  ViewAngleSlider := TViewAngleSlider.Create;

  { Note that Player is not created at this point.
    We will init Value of these sliders later. }
  RotationHorizontalSpeedSlider := TMenuFloatSlider.Create(25, 500, 1);
  RotationVerticalSpeedSlider := TMenuFloatSlider.Create(25, 500, 1);
  PlayerSpeedSlider := TMenuFloatSlider.Create(0.1, 5, 1);

  Items.Add('Infinite Life');
  Items.Add('Fly');
  Items.AddObject('Set view angle', ViewAngleSlider);
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
      MessageOK(Window, 'No can do. You are dead.', taLeft) else
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
    1: Player.FlyingModeTimeoutBegin(60 * 60);
    2: ;
    3: ;
    4: ;
    5: ;
    6: Player.LoadFromFile;
    7: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

procedure TDebugPlayerMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    2: begin
         ViewAngleDegX := ViewAngleSlider.Value;
         { After changing ViewAngleDegX, game's OnResize must be called. }
         Window.EventResize;
       end;
    3: Player.Camera.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
    4: Player.Camera.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
    5: Player.Camera.MoveSpeed := PlayerSpeedSlider.Value;
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
          C.SoundDyingEnabled := false;
          C.Hurt(1000 * 1000, ZeroVector3Single, 0);
        end;
      end;
  end;

  procedure AddLevelCreature(DirectionAttenuation: Single);
  var
    Kind: T3DResource;
  begin
    if ChooseResource(Kind, true) then
    begin
      (Kind as TCreatureKind).CreateCreature(SceneManager.Items,
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
      LevelsAvailable.SortByNumber;

      for I := 0 to LevelsAvailable.Count - 1 do
      begin
        S.Append(Format('Level %d "%s"',
          [ LevelsAvailable[I].Number, LevelsAvailable[I].Title ]));
      end;
      S.Append('Cancel');

      Index := ChooseByMenu(ControlsUnder, S);

      if Index <> LevelsAvailable.Count then
      begin
        LevelFinished(LevelsAvailable[Index].Id);
        { Flush LevelFinished now, to give new items when new level is loaded.
          Otherwise, some sounds (like equipping the sword, if player gets
          his first weapon) could be done before loading level progress,
          which sounds awkward for player. }
        LevelFinishedFlush;
        SceneManager.Level.PrepareNewPlayer(Player);
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

    LevelFinished(SceneManager.Info.Id);
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
    for I := 0 to AllResources.Count - 1 do
      if AllResources[I] is TItemKind then
        Player.PickItem(TItem.Create(TItemKind(AllResources[I]), 20));
    UserQuit := true;
  end;

begin
  inherited;

  case CurrentItem of
    0: GiveItems;
    1: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ TEditHeadlightMenu --------------------------------------------------------- }

constructor TEditHeadlightMenu.Create(AOwner: TComponent; AHeadlight: TAbstractLightNode);
begin
  inherited Create(AOwner);

  Headlight := AHeadlight;

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  AmbientIntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdAmbientIntensity.Value);

  ColorSlider[0] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[0]);
  ColorSlider[1] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[1]);
  ColorSlider[2] := TMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[2]);

  IntensitySlider := TMenuFloatSlider.Create(0, 1, Headlight.FdIntensity.Value);

  Items.AddObject('Ambient intensity'  , AmbientIntensitySlider);

  Items.AddObject('Color red'  , ColorSlider[0]);
  Items.AddObject('Color green', ColorSlider[1]);
  Items.AddObject('Color blue' , ColorSlider[2]);

  Items.AddObject('Intensity'  , IntensitySlider);

  Items.Add('Change attenuation');

  Items.Add('Back');
end;

procedure TEditHeadlightMenu.Click;

  procedure ChangeAttenuation;
  var
    Vector3: TVector3Single;
  begin
    if Headlight is TAbstractPositionalLightNode then
    begin
      Vector3 := TAbstractPositionalLightNode(Headlight).FdAttenuation.Value;
      if MessageInputQueryVector3Single(Window, 'Change headlight Attenuation',
        Vector3, taLeft) then
        TAbstractPositionalLightNode(Headlight).FdAttenuation.Value := Vector3;
    end else
      MessageOk(Window, 'Light is not positional, no attenuation');
  end;

begin
  case CurrentItem of
    0..4: Exit;
    5: ChangeAttenuation;
    6: SetCurrentMenu(CurrentMenu, EditLevelLightsMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TEditHeadlightMenu.AccessoryValueChanged;
begin
  case CurrentItem of
    0:    Headlight.FdAmbientIntensity.Value := AmbientIntensitySlider.Value;
    1..3: Headlight.FdColor.Value[CurrentItem-1] := ColorSlider[CurrentItem-1].Value;
    4:    Headlight.FdIntensity.Value := IntensitySlider.Value;
    else Exit;
  end;
end;

{ TEditLevelLightsMenu ------------------------------------------------------- }

{ GlobalAmbientLight is just a hack now, it is modified and directly set
  for OpenGL now.
  Default is equal to OpenGL default.

  This menu for editing lights may be moved
  one day to view3dscene, and control of global ambient may be changed
  to control NavigationInfo.globalAmbient field
  (InstantReality extension that we may implement too,
  see http://doc.instantreality.org/documentation/nodetype/NavigationInfo/ ). }

var
  GlobalAmbientLight: TVector3Single = (0.2, 0.2, 0.2);

constructor TEditLevelLightsMenu.Create(AOwner: TComponent);
var
  I: Integer;
  LightNode: TAbstractLightNode;
begin
  inherited;

  { To better visualize changes to lights }
  DrawBackgroundRectangle := false;

  AmbientColorSlider[0] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[0]);
  AmbientColorSlider[1] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[1]);
  AmbientColorSlider[2] := TMenuFloatSlider.Create(0, 1, GlobalAmbientLight[2]);

  for I := 0 to SceneManager.MainScene.GlobalLights.Count - 1 do
  begin
    LightNode := SceneManager.MainScene.GlobalLights.Items[I].Node;
    Items.Add(Format('Edit %d: %s "%s"',
      [I, LightNode.NodeTypeName, LightNode.NodeName]));
  end;
  Items.Add('Output level main file (with lights) on console');
  Items.Add('Save level main file (with lights)');
  Items.AddObject('Global ambient light red'  , AmbientColorSlider[0]);
  Items.AddObject('Global ambient light green', AmbientColorSlider[1]);
  Items.AddObject('Global ambient light blue' , AmbientColorSlider[2]);
  Items.Add('Edit headlight');
  Items.Add('Back to debug menu');
end;

procedure TEditLevelLightsMenu.Click;
const
  SaveGenerator = '"The Castle" lights editor, http://castle-engine.sourceforge.net/castle.php';
var
  H: TLightInstance;
begin
  case CurrentItem - SceneManager.MainScene.GlobalLights.Count of
    0: begin
         if StdOutStream <> nil then
           Save3D(SceneManager.MainScene.RootNode, StdOutStream,
             SaveGenerator, '', xeClassic) else
           MessageOK(Window, 'No stdout available. On Windows you must run the game ' +
             'from the command-line to get stdout.', taLeft);
       end;
    1: begin
         if MessageYesNo(Window, Format('This will permanently overwrite file "%s". ' +
           'Are you sure you want to save the level file ?',
           [SceneManager.Info.SceneFileName]), taLeft) then
           Save3D(SceneManager.MainScene.RootNode, SceneManager.Info.SceneFileName,
             SaveGenerator, '', xeClassic);
       end;
    2, 3, 4: ;
    5: begin
         if SceneManager.HeadlightInstance(H) then
         begin
           FreeAndNil(EditHeadlightMenu);
           EditHeadlightMenu := TEditHeadlightMenu.Create(Application, H.Node);
           SetCurrentMenu(CurrentMenu, EditHeadlightMenu);
         end else
           MessageOK(Window, 'No headlight in level ' +
             ' (set NavigationInfo.headlight to TRUE to get headlight)', taLeft);
       end;
    6: SetCurrentMenu(CurrentMenu, DebugMenu);
    else
       begin
         FreeAndNil(EditOneLightMenu);
         EditOneLightMenu := TEditOneLightMenu.Create(nil,
           SceneManager.MainScene.GlobalLights.Items[CurrentItem].Node);
         SetCurrentMenu(CurrentMenu, EditOneLightMenu);
       end;
  end;
end;

procedure TEditLevelLightsMenu.AccessoryValueChanged;
begin
  case CurrentItem - SceneManager.MainScene.GlobalLights.Count of
    2: GlobalAmbientLight[0] := AmbientColorSlider[0].Value;
    3: GlobalAmbientLight[1] := AmbientColorSlider[1].Value;
    4: GlobalAmbientLight[2] := AmbientColorSlider[2].Value;
    else Exit;
  end;

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Vector4Single(GlobalAmbientLight, 1.0));
end;

{ TEditOneLightMenu ---------------------------------------------------------- }

constructor TEditOneLightMenu.Create(AOwner: TComponent; ALight: TAbstractLightNode);
var
  I: Integer;
  LevelBoxSizes: TVector3Single;
begin
  inherited Create(AOwner);

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  Light := ALight;

  LevelBoxSizes := SceneManager.CameraBox.Sizes;
  for I := 0 to 2 do
    PositionSlider[I] := TMenuFloatSlider.Create(
      SceneManager.CameraBox.Data[0, I] - LevelBoxSizes[I],
      SceneManager.CameraBox.Data[1, I] + LevelBoxSizes[I],
      LightLocation[I]);

  RedColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[0]);
  GreenColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[1]);
  BlueColorSlider := TMenuFloatSlider.Create(0, 1, Light.FdColor.Value[2]);
  IntensitySlider := TMenuFloatSlider.Create(0, 1, Light.FdIntensity.Value);
  AmbientIntensitySlider := TMenuFloatSlider.Create(
    -1, 1, Light.FdAmbientIntensity.Value);
  OnArgument := TMenuBooleanArgument.Create(Light.FdOn.Value);
  ShadowsArgument := TMenuBooleanArgument.Create(Light.FdKambiShadows.Value);
  ShadowsMainArgument := TMenuBooleanArgument.Create(
    Light.FdKambiShadowsMain.Value);

  Items.AddObject('Position X', PositionSlider[0]);
  Items.AddObject('Position Y', PositionSlider[1]);
  Items.AddObject('Position Z', PositionSlider[2]);
  Items.AddObject('Red color', RedColorSlider);
  Items.AddObject('Green color', GreenColorSlider);
  Items.AddObject('Blue color', BlueColorSlider);
  Items.AddObject('Intensity', IntensitySlider);
  Items.AddObject('Ambient intensity', AmbientIntensitySlider);
  Items.AddObject('On', OnArgument);
  Items.AddObject('Shadow Vols', ShadowsArgument);
  Items.AddObject('Shadows Vols Main', ShadowsMainArgument);
  Items.Add('Point/SpotLight: Change attenuation');
  Items.Add('DirectionalLight: Change direction');
  Items.Add('SpotLight: Change direction');
  Items.Add('SpotLight: Change beamWidth/dropOffRate');
  Items.Add('SpotLight: Change cutOffAngle');
  Items.Add('Back');
end;

function TEditOneLightMenu.GetLightLocation: TVector3Single;
begin
  if Light is TAbstractPositionalLightNode then
    Result := TAbstractPositionalLightNode(Light).FdLocation.Value else
    Result := ZeroVector3Single;
end;

procedure TEditOneLightMenu.SetLightLocation(const Value: TVector3Single);
begin
  if Light is TAbstractPositionalLightNode then
    TAbstractPositionalLightNode(Light).FdLocation.Value := Value;
end;

procedure TEditOneLightMenu.Click;

  function MessageInputQueryVector3SingleP(
    Window: TCastleWindowBase; const Title: string;
    var Value: TVector3Single; TextAlign: TTextAlign;
    const OnP: TVector3Single): boolean;
  var s: string;
  begin
   Result := false;
   s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
   if MessageInputQuery(Window, Title, s, TextAlign) then
   begin
    try
     if LowerCase(Trim(S)) = 'p' then
       Value := OnP else
       Value := Vector3SingleFromStr(s);
     Result := true;
    except
     on E: EConvertError do
     begin
      MessageOK(Window, 'Invalid vector 3 value : ' + E.Message, taLeft);
     end;
    end;
   end;
  end;

var
  Vector: TVector3Single;
  Value: Single;
begin
  case CurrentItem of
    0..7: ;
    8: begin
         OnArgument.Value := not OnArgument.Value;
         Light.FdOn.Send(OnArgument.Value);
       end;
    9: begin
         ShadowsArgument.Value := not ShadowsArgument.Value;
         Light.FdKambiShadows.Send(ShadowsArgument.Value);
       end;
    10:begin
         ShadowsMainArgument.Value := not ShadowsMainArgument.Value;
         Light.FdKambiShadowsMain.Send(ShadowsMainArgument.Value);
       end;
    11:begin
         if Light is TAbstractPositionalLightNode then
         begin
           Vector := TAbstractPositionalLightNode(Light).FdAttenuation.Value;
           if MessageInputQueryVector3Single(Window, 'Change attenuation',
             Vector, taLeft) then
             TAbstractPositionalLightNode(Light).FdAttenuation.Send(Vector);
         end;
       end;
    12:begin
         if Light is TAbstractDirectionalLightNode then
         begin
           Vector := TAbstractDirectionalLightNode(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Direction) then
             TAbstractDirectionalLightNode(Light).FdDirection.Send(Vector);
         end;
       end;
    13:begin
         if Light is TSpotLightNode_1 then
         begin
           Vector := TSpotLightNode_1(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Direction) then
             TSpotLightNode_1(Light).FdDirection.Send(Vector);
         end else
         if Light is TSpotLightNode then
         begin
           Vector := TSpotLightNode(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Direction) then
             TSpotLightNode(Light).FdDirection.Send(Vector);
         end;
       end;
    14:begin
         if Light is TSpotLightNode_1 then
         begin
           Value := TSpotLightNode_1(Light).FdDropOffRate.Value;
           if MessageInputQuery(Window, 'Change dropOffRate', Value, taLeft) then
             TSpotLightNode_1(Light).FdDropOffRate.Send(Value);
         end else
         if Light is TSpotLightNode then
         begin
           Value := TSpotLightNode(Light).FdBeamWidth.Value;
           if MessageInputQuery(Window, 'Change beamWidth', Value, taLeft) then
             TSpotLightNode(Light).FdBeamWidth.Send(Value);
         end;
       end;
    15:begin
         if Light is TSpotLightNode_1 then
         begin
           Value := TSpotLightNode_1(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TSpotLightNode_1(Light).FdCutOffAngle.Send(Value);
         end else
         if Light is TSpotLightNode then
         begin
           Value := TSpotLightNode(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TSpotLightNode(Light).FdCutOffAngle.Send(Value);
         end;
       end;
    16:SetCurrentMenu(CurrentMenu, EditLevelLightsMenu);
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TEditOneLightMenu.AccessoryValueChanged;
var
  Index: Integer;
  V: TVector3Single;
begin
  case CurrentItem of
    0..2:
      if Light is TAbstractPositionalLightNode then
      begin
        Index := CurrentItem;
        V := TAbstractPositionalLightNode(Light).FdLocation.Value;
        V[Index] := PositionSlider[Index].Value;
        TAbstractPositionalLightNode(Light).FdLocation.Send(V);
      end;
    3: begin Light.FdColor.Value[0] := RedColorSlider.Value  ; Light.FdColor.Changed; end;
    4: begin Light.FdColor.Value[1] := GreenColorSlider.Value; Light.FdColor.Changed; end;
    5: begin Light.FdColor.Value[2] := BlueColorSlider.Value ; Light.FdColor.Changed; end;
    6: Light.FdIntensity.Send(IntensitySlider.Value);
    7: Light.FdAmbientIntensity.Send(AmbientIntensitySlider.Value);
    else Exit;
  end;
end;

{ global things -------------------------------------------------------------- }

{$I gamemenucallbacks.inc}

procedure ShowDebugMenu(AControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
begin
  ControlsUnder := AControlsUnder;

  DebugPlayerMenu.RotationHorizontalSpeedSlider.Value :=
    Player.Camera.RotationHorizontalSpeed;
  DebugPlayerMenu.RotationVerticalSpeedSlider.Value :=
    Player.Camera.RotationVerticalSpeed;
  DebugPlayerMenu.PlayerSpeedSlider.Value := Player.Camera.MoveSpeed;

  SavedMode := TGLMode.CreateReset(Window, 0, true,
    nil, Window.OnResize, @CloseQuery);
  try
    { This is needed, because when changing ViewAngleDegX we will call
      Window.OnResize to set new projection matrix, and this
      new projection matrix should stay for the game. }
    SavedMode.RestoreProjectionMatrix := false;

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    MessagesTheme.RectColor[3] := 1.0;

    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnMouseWheel := @MouseWheel;
    Window.OnDrawStyle := ds3D;

    SetCurrentMenu(CurrentMenu, DebugMenu);

    Window.Controls.AddList(ControlsUnder);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true, true);
    until GameEnded or UserQuit;
  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(const Container: IUIContainer);
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
  OnGLContextOpen.Add(@WindowOpen);
finalization
  { This may be left created }
  FreeAndNil(EditOneLightMenu);
end.
