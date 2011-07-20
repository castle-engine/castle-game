{
  Copyright 2006-2011 Michalis Kamburelis.

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
unit CastleDebugMenu;

interface

uses UIControls;

procedure ShowDebugMenu(AControlsUnder: TUIControlList);

implementation

uses SysUtils, Classes, KambiUtils, KambiStringUtils, GLWinModes,
  GL, GLU, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, GLWindow, CastlePlay, CastleGeneralMenu,
  CastleControlsMenu, CastleInputs, CastleCreatures, CastleChooseMenu,
  CastleItems, GLMenu, RaysWindow, CastleVideoOptions,
  CastleSound, VRMLNodes, KambiClassUtils, CastleNotifications,
  CastleLevelAvailable, Boxes3D, KeysMouse;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TViewAngleSlider = class(TGLMenuFloatSlider)
  public
    constructor Create;
    function ValueToStr(const AValue: Single): string; override;
  end;

  TDebugMenu = class(TCastleMenu)
  public
    RenderBoundingBoxesArgument: TGLMenuBooleanArgument;
    RenderDebugCaptionsArgument: TGLMenuBooleanArgument;
    DebugRenderShadowVolumeArgument: TGLMenuBooleanArgument;
    DebugRenderForLevelScreenshotArgument: TGLMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugPlayerMenu = class(TCastleMenu)
  public
    ViewAngleSlider: TViewAngleSlider;
    RotationHorizontalSpeedSlider: TGLMenuFloatSlider;
    RotationVerticalSpeedSlider: TGLMenuFloatSlider;
    PlayerSpeedSlider: TGLMenuFloatSlider;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TDebugItemsMenu = class(TCastleMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugCreaturesMenu = class(TCastleMenu)
  public
    DebugTimeStopForCreaturesArgument: TGLMenuBooleanArgument;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TDebugLevelMenu = class(TCastleMenu)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TEditLevelLightsMenu = class(TCastleMenu)
  public
    AmbientColorSlider: array[0..2] of TGLMenuFloatSlider;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    procedure AccessoryValueChanged; override;
  end;

  TEditOneLightMenu = class(TCastleMenu)
  public
    Light: TNodeX3DLightNode;
    RedColorSlider: TGLMenuFloatSlider;
    GreenColorSlider: TGLMenuFloatSlider;
    BlueColorSlider: TGLMenuFloatSlider;
    IntensitySlider: TGLMenuFloatSlider;
    AmbientIntensitySlider: TGLMenuFloatSlider;
    OnArgument: TGLMenuBooleanArgument;
    ShadowsArgument: TGLMenuBooleanArgument;
    ShadowsMainArgument: TGLMenuBooleanArgument;
    PositionSlider: array [0..2] of TGLMenuFloatSlider;
    constructor Create(AOwner: TComponent; ALight: TNodeX3DLightNode); reintroduce;
    procedure Click; override;
    procedure AccessoryValueChanged; override;

    function GetLightLocation: TVector3Single;
    procedure SetLightLocation(const Value: TVector3Single);
    property LightLocation: TVector3Single
      read GetLightLocation write SetLightLocation;
  end;

  TEditHeadlightMenu = class(TCastleMenu)
  public
    Headlight: TNodeX3DLightNode;
    AmbientIntensitySlider: TGLMenuFloatSlider;
    ColorSlider: array[0..2] of TGLMenuFloatSlider;
    IntensitySlider: TGLMenuFloatSlider;
    SpotArgument: TGLMenuBooleanArgument;
    constructor Create(AOwner: TComponent; AHeadlight: TNodeX3DLightNode); reintroduce;
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
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  ControlsUnder: TUIControlList;
  CurrentMenu: TCastleMenu;

  DebugMenu: TDebugMenu;
  DebugPlayerMenu: TDebugPlayerMenu;
  DebugCreaturesMenu: TDebugCreaturesMenu;
  DebugLevelMenu: TDebugLevelMenu;
  DebugItemsMenu: TDebugItemsMenu;
  EditLevelLightsMenu: TEditLevelLightsMenu;
  EditOneLightMenu: TEditOneLightMenu;
  EditHeadlightMenu: TEditHeadlightMenu;

{ TDebugMenu ------------------------------------------------------------ }

constructor TDebugMenu.Create(AOwner: TComponent);
begin
  inherited;

  RenderBoundingBoxesArgument := TGLMenuBooleanArgument.Create(RenderBoundingBoxes);
  RenderDebugCaptionsArgument := TGLMenuBooleanArgument.Create(RenderDebugCaptions);
  DebugRenderShadowVolumeArgument := TGLMenuBooleanArgument.Create(DebugRenderShadowVolume);
  DebugRenderForLevelScreenshotArgument := TGLMenuBooleanArgument.Create(
    DebugRenderForLevelScreenshot);

  Items.Add('Player debug menu');
  Items.Add('Creatures debug menu');
  Items.Add('Items debug menu');
  Items.Add('Level debug menu');
  Items.AddObject('Render bounding boxes', RenderBoundingBoxesArgument);
  Items.AddObject('Render debug captions', RenderDebugCaptionsArgument);
  Items.AddObject('Render shadow volumes', DebugRenderShadowVolumeArgument);
  Items.AddObject('Render for level screenshot',
    DebugRenderForLevelScreenshotArgument);
  Items.Add('Reload sounds/index.xml');
  Items.Add('Edit lights');
  Items.Add('Force thunder now');
  Items.Add('Back to game');
end;

procedure TDebugMenu.Click;

  procedure ForceThunder;
  begin
    if Level.ThunderEffect <> nil then
    begin
      Level.ThunderEffect.ForceNow;
      UserQuit := true;
    end else
      MessageOK(Window, 'Thunder effect not defined for this level.', taLeft);
  end;

begin
  inherited;

  case CurrentItem of
    0: SetCurrentMenu(CurrentMenu, DebugPlayerMenu);
    1: SetCurrentMenu(CurrentMenu, DebugCreaturesMenu);
    2: SetCurrentMenu(CurrentMenu, DebugItemsMenu);
    3: SetCurrentMenu(CurrentMenu, DebugLevelMenu);
    4: begin
         RenderBoundingBoxes := not RenderBoundingBoxes;
         RenderBoundingBoxesArgument.Value := RenderBoundingBoxes;
       end;
    5: begin
         RenderDebugCaptions := not RenderDebugCaptions;
         RenderDebugCaptionsArgument.Value := RenderDebugCaptions;
       end;
    6: begin
         DebugRenderShadowVolume := not DebugRenderShadowVolume;
         DebugRenderShadowVolumeArgument.Value := DebugRenderShadowVolume;
       end;
    7: begin
         DebugRenderForLevelScreenshot := not DebugRenderForLevelScreenshot;
         DebugRenderForLevelScreenshotArgument.Value :=
           DebugRenderForLevelScreenshot;
       end;
    8: SoundEngine.ReadSoundInfos;
    9: begin
         FreeAndNil(EditLevelLightsMenu);
         EditLevelLightsMenu := TEditLevelLightsMenu.Create(Application);
         SetCurrentMenu(CurrentMenu, EditLevelLightsMenu);
       end;
    10:ForceThunder;
    11:UserQuit := true;
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
  RotationHorizontalSpeedSlider := TGLMenuFloatSlider.Create(25, 500, 1);
  RotationVerticalSpeedSlider := TGLMenuFloatSlider.Create(25, 500, 1);
  PlayerSpeedSlider := TGLMenuFloatSlider.Create(0.1, 5, 1);

  Items.Add('Set Player.MaxLife');
  Items.Add('Player.Life := Player.MaxLife');
  Items.AddObject('Set view angle', ViewAngleSlider);
  Items.AddObject('Set horizontal rotation speed', RotationHorizontalSpeedSlider);
  Items.AddObject('Set vertical rotation speed', RotationVerticalSpeedSlider);
  Items.AddObject('Set player speed', PlayerSpeedSlider);
  Items.Add('Reload player.xml file');
  Items.Add('Fly');
  Items.Add('Back');
end;

procedure TDebugPlayerMenu.Click;

  procedure PlayerSetMaxLife;
  var
    Value: Single;
  begin
    Value := Player.MaxLife;
    if MessageInputQuery(Window, 'Set Player.MaxLife',
      Value, taLeft) then
      Player.MaxLife := Value;
  end;

  procedure PlayerMaxLife;
  begin
    if Player.Dead then
      MessageOK(Window, 'No can do. You are dead.', taLeft) else
    begin
      Player.Life := Player.MaxLife;
      UserQuit := true;
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: PlayerSetMaxLife;
    1: PlayerMaxLife;
    2: ;
    3: ;
    4: ;
    5: ;
    6: Player.LoadFromFile;
    7: Player.FlyingModeTimeoutBegin(60 * 60);
    8: SetCurrentMenu(CurrentMenu, DebugMenu);
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

  DebugTimeStopForCreaturesArgument := TGLMenuBooleanArgument.Create(
    DebugTimeStopForCreatures);

  Items.Add('Show info about creatures on level');
  Items.Add('Kill all creatures');
  Items.Add('Kill all non-still creatures');
  Items.Add('Add creature to level before player');
  Items.Add('Add creature to level exactly on player');
  Items.Add('Reload creatures/kinds.xml file');
  Items.Add('Reload animations of specific creature');
  Items.AddObject('Time stop for creatures', DebugTimeStopForCreaturesArgument);
  Items.Add('Back');
end;

procedure TDebugCreaturesMenu.Click;

  function ChooseCreatureKind(out ChooseCreature: TCreatureKind): boolean;
  var
    S: TStringList;
    I, ResultIndex: Integer;
  begin
    S := TStringList.Create;
    try
      for I := 0 to CreaturesKinds.High do
        S.Append(Format('Creature %s (%d users)',
          [CreaturesKinds[I].VRMLNodeName, CreaturesKinds[I].RequiredCount]));
      S.Append('Cancel');
      ResultIndex := ChooseByMenu(ControlsUnder, S);
      Result := ResultIndex <> CreaturesKinds.High + 1;
      if Result then
        ChooseCreature := CreaturesKinds[ResultIndex];
    finally S.Free end;
  end;

  procedure ShowLevelCreaturesInfo;
  var
    S: TStringList;
    I: Integer;
  begin
    S := TStringList.Create;
    try
      S.Append(Format('%d creatures on level:', [Level.Creatures.Count]));
      S.Append('Index: Kind, Position, Life / MaxLife, CameraRadius');
      S.Append('');

      for I := 0 to Level.Creatures.High do
        S.Append(Format('%d: %s, %s, %s / %s, %s',
          [ I, Level.Creatures[I].Kind.VRMLNodeName,
            VectorToNiceStr(Level.Creatures[I].LegsPosition),
            FloatToNiceStr(Level.Creatures[I].Life),
            FloatToNiceStr(Level.Creatures[I].MaxLife),
            FloatToNiceStr(Level.Creatures[I].Kind.CameraRadius) ]));

      MessageOK(Window, S, taLeft);
    finally S.Free end;
  end;

  procedure KillAll;
  var
    I: Integer;
  begin
    for I := 0 to Level.Creatures.High do
      if Level.Creatures[I].Life > 0 then
      begin
        Level.Creatures[I].SoundDyingEnabled := false;
        Level.Creatures[I].Life := 0;
        Level.Creatures[I].LastAttackDirection := ZeroVector3Single;
      end;
  end;

  procedure KillAllNonStill;
  var
    I: Integer;
  begin
    for I := 0 to Level.Creatures.High do
      if (not (Level.Creatures[I] is TStillCreature)) and
        (Level.Creatures[I].Life > 0) then
      begin
        Level.Creatures[I].SoundDyingEnabled := false;
        Level.Creatures[I].Life := 0;
        Level.Creatures[I].LastAttackDirection := ZeroVector3Single;
      end;
  end;

  procedure AddLevelCreature(DirectionAttenuation: Single);
  var
    Position: TVector3Single;
    Direction: TVector3Single;
    Kind: TCreatureKind;
  begin
    if ChooseCreatureKind(Kind) then
    begin
      Position := VectorAdd(Player.Camera.Position,
        VectorScale(Player.Camera.Direction, DirectionAttenuation));
      Direction := Player.Camera.Direction;

      Level.Creatures.Add(
        Kind.CreateDefaultCreature(Position, Direction, Level.AnimationTime,
          Level.BaseLights, Kind.DefaultMaxLife));

      UserQuit := true;
    end;
  end;

  procedure ReloadCreatureAnimation;
  var
    Kind: TCreatureKind;
  begin
    if ChooseCreatureKind(Kind) then
    begin
      if Kind.RequiredCount = 0 then
        MessageOK(Window, Format('Creature "%s" is not used by anything, ' +
          'cannot reload',  [Kind.VRMLNodeName])) else
        Kind.RedoPrepareRender(Level.BaseLights);
    end;
  end;

begin
  inherited;

  case CurrentItem of
    0: ShowLevelCreaturesInfo;
    1: KillAll;
    2: KillAllNonStill;
    3: AddLevelCreature(10);
    4: AddLevelCreature(0);
    5: CreaturesKinds.LoadFromFile;
    6: ReloadCreatureAnimation;
    7: begin
         DebugTimeStopForCreatures := not DebugTimeStopForCreatures;
         DebugTimeStopForCreaturesArgument.Value := DebugTimeStopForCreatures;
       end;
    8: SetCurrentMenu(CurrentMenu, DebugMenu);
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

      for I := 0 to LevelsAvailable.High do
      begin
        S.Append(Format('Level %d "%s"',
          [ LevelsAvailable[I].Number, LevelsAvailable[I].Title ]));
      end;
      S.Append('Cancel');

      Index := ChooseByMenu(ControlsUnder, S);

      if Index <> LevelsAvailable.Count then
      begin
        LevelFinished(LevelsAvailable[Index].Name);
        { Flush LevelFinished now, to give new items when new level is loaded.
          Otherwise, some sounds (like equipping the sword, if player gets
          his first weapon) could be done before loading level progress,
          which sounds awkward for player. }
        LevelFinishedFlush;
        Level.PrepareNewPlayer(Player);
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

    LevelFinished(Level.Name);
    LevelFinishedFlush;

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
  Items.Add('Reload items/kinds.xml file');
  Items.Add('Reload animations/models of specific item');
  Items.Add('Back');
end;

procedure TDebugItemsMenu.Click;

  function ChooseItemKind(out ChooseItem: TItemKind): boolean;
  var
    S: TStringList;
    I, ResultIndex: Integer;
  begin
    S := TStringList.Create;
    try
      for I := 0 to ItemsKinds.High do
        S.Append('Item ' + ItemsKinds[I].VRMLNodeName);
      S.Append('Cancel');
      ResultIndex := ChooseByMenu(ControlsUnder, S);
      Result := ResultIndex <> ItemsKinds.High + 1;
      if Result then
        ChooseItem := ItemsKinds[ResultIndex];
    finally S.Free end;
  end;

  procedure GiveItems;
  var
    I: Integer;
  begin
    for I := 0 to ItemsKinds.High do
      Player.PickItem(TItem.Create(ItemsKinds[I], 20));
    UserQuit := true;
  end;

  procedure ReloadItemAnimation;
  var
    Kind: TItemKind;
  begin
    if ChooseItemKind(Kind) then
      Kind.RedoPrepareRender(Level.BaseLights);
  end;

begin
  inherited;

  case CurrentItem of
    0: GiveItems;
    1: ItemsKinds.LoadFromFile;
    2: ReloadItemAnimation;
    3: SetCurrentMenu(CurrentMenu, DebugMenu);
  end;
end;

{ TEditHeadlightMenu --------------------------------------------------------- }

constructor TEditHeadlightMenu.Create(AOwner: TComponent; AHeadlight: TNodeX3DLightNode);
begin
  inherited Create(AOwner);

  Headlight := AHeadlight;

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  AmbientIntensitySlider := TGLMenuFloatSlider.Create(0, 1, Headlight.FdAmbientIntensity.Value);

  ColorSlider[0] := TGLMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[0]);
  ColorSlider[1] := TGLMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[1]);
  ColorSlider[2] := TGLMenuFloatSlider.Create(0, 1, Headlight.FdColor.Value[2]);

  IntensitySlider := TGLMenuFloatSlider.Create(0, 1, Headlight.FdIntensity.Value);

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
    if Headlight is TVRMLPositionalLightNode then
    begin
      Vector3 := TVRMLPositionalLightNode(Headlight).FdAttenuation.Value;
      if MessageInputQueryVector3Single(Window, 'Change headlight Attenuation',
        Vector3, taLeft) then
        TVRMLPositionalLightNode(Headlight).FdAttenuation.Value := Vector3;
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

constructor TEditLevelLightsMenu.Create(AOwner: TComponent);
var
  I: Integer;
  LightNode: TNodeX3DLightNode;
begin
  inherited;

  { To better visualize changes to Level.GlobalAmbientLight }
  DrawBackgroundRectangle := false;

  AmbientColorSlider[0] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[0]);
  AmbientColorSlider[1] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[1]);
  AmbientColorSlider[2] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[2]);

  for I := 0 to Level.MainScene.GlobalLights.High do
  begin
    LightNode := Level.MainScene.GlobalLights.Items[I].Node;
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
  SaveGenerator = '"The Castle" lights editor, http://vrmlengine.sourceforge.net/castle.php';
var
  H: TLightInstance;
begin
  case CurrentItem - Level.MainScene.GlobalLights.Count of
    0: begin
         if StdOutStream <> nil then
           SaveVRML(Level.MainScene.RootNode, StdOutStream,
             SaveGenerator, '', xeClassic) else
           MessageOK(Window, 'No stdout available. On Windows you must run the game ' +
             'from the command-line to get stdout.', taLeft);
       end;
    1: begin
         if MessageYesNo(Window, Format('This will permanently overwrite file "%s". ' +
           'Are you sure you want to save the level file ?',
           [Level.SceneFileName]), taLeft) then
           SaveVRML(Level.MainScene.RootNode, Level.SceneFileName,
             SaveGenerator, '', xeClassic);
       end;
    2, 3, 4: ;
    5: begin
         if Level.HeadlightInstance(H) then
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
           Level.MainScene.GlobalLights.Items[CurrentItem].Node);
         SetCurrentMenu(CurrentMenu, EditOneLightMenu);
       end;
  end;
end;

procedure TEditLevelLightsMenu.AccessoryValueChanged;
begin
  case CurrentItem - Level.MainScene.GlobalLights.Count of
    2: Level.GlobalAmbientLight[0] := AmbientColorSlider[0].Value;
    3: Level.GlobalAmbientLight[1] := AmbientColorSlider[1].Value;
    4: Level.GlobalAmbientLight[2] := AmbientColorSlider[2].Value;
    else Exit;
  end;

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Level.GlobalAmbientLight);
end;

{ TEditOneLightMenu ---------------------------------------------------------- }

constructor TEditOneLightMenu.Create(AOwner: TComponent; ALight: TNodeX3DLightNode);
var
  I: Integer;
  LevelBoxSizes: TVector3Single;
begin
  inherited Create(AOwner);

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  Light := ALight;

  LevelBoxSizes := Box3DSizes(Level.CameraBox);
  for I := 0 to 2 do
    PositionSlider[I] := TGLMenuFloatSlider.Create(
      Level.CameraBox[0, I] - LevelBoxSizes[I],
      Level.CameraBox[1, I] + LevelBoxSizes[I],
      LightLocation[I]);

  RedColorSlider := TGLMenuFloatSlider.Create(0, 1, Light.FdColor.Value[0]);
  GreenColorSlider := TGLMenuFloatSlider.Create(0, 1, Light.FdColor.Value[1]);
  BlueColorSlider := TGLMenuFloatSlider.Create(0, 1, Light.FdColor.Value[2]);
  IntensitySlider := TGLMenuFloatSlider.Create(0, 1, Light.FdIntensity.Value);
  AmbientIntensitySlider := TGLMenuFloatSlider.Create(
    -1, 1, Light.FdAmbientIntensity.Value);
  OnArgument := TGLMenuBooleanArgument.Create(Light.FdOn.Value);
  ShadowsArgument := TGLMenuBooleanArgument.Create(Light.FdKambiShadows.Value);
  ShadowsMainArgument := TGLMenuBooleanArgument.Create(
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
  Items.AddObject('Shadows', ShadowsArgument);
  Items.AddObject('Shadows main light', ShadowsMainArgument);
  Items.Add('Point/SpotLight: Change attenuation');
  Items.Add('DirectionalLight: Change direction');
  Items.Add('SpotLight: Change direction');
  Items.Add('SpotLight: Change beamWidth/dropOffRate');
  Items.Add('SpotLight: Change cutOffAngle');
  Items.Add('Back');
end;

function TEditOneLightMenu.GetLightLocation: TVector3Single;
begin
  if Light is TVRMLPositionalLightNode then
    Result := TVRMLPositionalLightNode(Light).FdLocation.Value else
    Result := ZeroVector3Single;
end;

procedure TEditOneLightMenu.SetLightLocation(const Value: TVector3Single);
begin
  if Light is TVRMLPositionalLightNode then
    TVRMLPositionalLightNode(Light).FdLocation.Value := Value;
end;

procedure TEditOneLightMenu.Click;

  function MessageInputQueryVector3SingleP(
    Window: TGLWindow; const Title: string;
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
         if Light is TVRMLPositionalLightNode then
         begin
           Vector := TVRMLPositionalLightNode(Light).FdAttenuation.Value;
           if MessageInputQueryVector3Single(Window, 'Change attenuation',
             Vector, taLeft) then
             TVRMLPositionalLightNode(Light).FdAttenuation.Send(Vector);
         end;
       end;
    12:begin
         if Light is TVRMLDirectionalLightNode then
         begin
           Vector := TVRMLDirectionalLightNode(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Camera.Direction) then
             TVRMLDirectionalLightNode(Light).FdDirection.Send(Vector);
         end;
       end;
    13:begin
         if Light is TNodeSpotLight_1 then
         begin
           Vector := TNodeSpotLight_1(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Camera.Direction) then
             TNodeSpotLight_1(Light).FdDirection.Send(Vector);
         end else
         if Light is TNodeSpotLight then
         begin
           Vector := TNodeSpotLight(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Window, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Camera.Direction) then
             TNodeSpotLight(Light).FdDirection.Send(Vector);
         end;
       end;
    14:begin
         if Light is TNodeSpotLight_1 then
         begin
           Value := TNodeSpotLight_1(Light).FdDropOffRate.Value;
           if MessageInputQuery(Window, 'Change dropOffRate', Value, taLeft) then
             TNodeSpotLight_1(Light).FdDropOffRate.Send(Value);
         end else
         if Light is TNodeSpotLight then
         begin
           Value := TNodeSpotLight(Light).FdBeamWidth.Value;
           if MessageInputQuery(Window, 'Change beamWidth', Value, taLeft) then
             TNodeSpotLight(Light).FdBeamWidth.Send(Value);
         end;
       end;
    15:begin
         if Light is TNodeSpotLight_1 then
         begin
           Value := TNodeSpotLight_1(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TNodeSpotLight_1(Light).FdCutOffAngle.Send(Value);
         end else
         if Light is TNodeSpotLight then
         begin
           Value := TNodeSpotLight(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Window, 'Change cutOffAngle', Value, taLeft) then
             TNodeSpotLight(Light).FdCutOffAngle.Send(Value);
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
      if Light is TVRMLPositionalLightNode then
      begin
        Index := CurrentItem;
        V := TVRMLPositionalLightNode(Light).FdLocation.Value;
        V[Index] := PositionSlider[Index].Value;
        TVRMLPositionalLightNode(Light).FdLocation.Send(V);
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

{$I castlemenucallbacks.inc}

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
    nil, Window.OnResize, @CloseQuery,
    true { FPSActive should not be needed anymore, but I leave it. });
  try
    { This is needed, because when changing ViewAngleDegX we will call
      Window.OnResize to set new projection matrix, and this
      new projection matrix should stay for the game. }
    SavedMode.RestoreProjectionMatrix := false;

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    GLWinMessagesTheme.RectColor[3] := 1.0;

    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnMouseWheel := @MouseWheel;
    Window.OnIdle := @Idle;
    Window.OnDrawStyle := ds3D;

    SetCurrentMenu(CurrentMenu, DebugMenu);

    Window.Controls.AddList(ControlsUnder);

    UserQuit := false;
    repeat
      Application.ProcessMessage(true);
    until GameEnded or UserQuit;
  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure OpenWindow(Window: TGLWindow);
begin
  { Although base TGLMenu doesn't require OpenGL context at constructor,
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
  Window.OnOpenList.Add(@OpenWindow);
finalization
  { This may be left created }
  FreeAndNil(EditOneLightMenu);
end.
