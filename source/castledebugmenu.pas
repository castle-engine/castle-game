{
  Copyright 2006,2007 Michalis Kamburelis.

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
}

{ }
unit CastleDebugMenu;

interface

uses GLWindow;

procedure ShowDebugMenu(ADrawUnderMenu: TDrawFunc);

implementation

uses SysUtils, Classes, KambiUtils, KambiStringUtils, GLWinModes,
  GL, GLU, GLExt, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, CastleHelp, CastlePlay, CastleGeneralMenu,
  CastleControlsMenu, CastleInputs, CastleCreatures, CastleChooseMenu,
  CastleItems, GLMenu, RaysWindow, CastleVideoOptions, CastleLevel,
  CastleSound, VRMLNodes, KambiClassUtils, CastleTimeMessages,
  CastleLevelAvailable;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TViewAngleSlider = class(TGLMenuFloatSlider)
    constructor Create;
    function ValueToStr(const AValue: Single): string; override;
  end;

  TDebugMenu = class(TCastleMenu)
    RenderBoundingBoxesArgument: TGLMenuBooleanArgument;
    RenderDebugCaptionsArgument: TGLMenuBooleanArgument;
    DebugRenderShadowVolumeArgument: TGLMenuBooleanArgument;
    DebugRenderForLevelScreenshotArgument: TGLMenuBooleanArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TDebugPlayerMenu = class(TCastleMenu)
    ViewAngleSlider: TViewAngleSlider;
    RotationHorizontalSpeedSlider: TGLMenuFloatSlider;
    RotationVerticalSpeedSlider: TGLMenuFloatSlider;
    PlayerSpeedSlider: TGLMenuFloatSlider;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TDebugItemsMenu = class(TCastleMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TDebugCreaturesMenu = class(TCastleMenu)
    DebugTimeStopForCreaturesArgument: TGLMenuBooleanArgument;
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TDebugLevelMenu = class(TCastleMenu)
    LevelOctreeMaxDepthSlider: TGLMenuIntegerSlider;
    LevelOctreeMaxLeafItemsCountSlider: TGLMenuIntegerSlider;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TEditLevelLightsMenu = class(TCastleMenu)
    AmbientColorSlider: array[0..2] of TGLMenuFloatSlider;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TEditOneLightMenu = class(TCastleMenu)
    Light: TNodeGeneralLight;
    RedColorSlider: TGLMenuFloatSlider;
    GreenColorSlider: TGLMenuFloatSlider;
    BlueColorSlider: TGLMenuFloatSlider;
    IntensitySlider: TGLMenuFloatSlider;
    AmbientIntensitySlider: TGLMenuFloatSlider;
    OnArgument: TGLMenuBooleanArgument;
    ShadowsArgument: TGLMenuBooleanArgument;
    ShadowsMainArgument: TGLMenuBooleanArgument;
    constructor Create(ALight: TNodeGeneralLight);
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

  TEditHeadlightMenu = class(TCastleMenu)
    AmbientIntensitySlider: TGLMenuFloatSlider;
    ColorSlider: array[0..2] of TGLMenuFloatSlider;
    IntensitySlider: TGLMenuFloatSlider;
    SpotArgument: TGLMenuBooleanArgument;
    { Create this only when Level.Headlight <> nil. }
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

{ TViewAngleSlider ----------------------------------------------------------- }

constructor TViewAngleSlider.Create;
begin
  inherited Create(10, 170, ViewAngleDegX);
end;

function TViewAngleSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('horiz %f, vert %f', [AValue,
    AdjustViewAngleDegToAspectRatio(AValue, Glw.Height / Glw.Width)]);
end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  DrawUnderMenu: TDrawFunc;
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

constructor TDebugMenu.Create;
begin
  inherited Create;

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

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugMenu.CurrentItemSelected;

  procedure ForceThunder;
  begin
    if Level.ThunderEffect <> nil then
    begin
      Level.ThunderEffect.ForceNow;
      UserQuit := true;
    end else
      MessageOK(Glw, 'Thunder effect not defined for this level.', taLeft);
  end;

begin
  inherited;

  case CurrentItem of
    0: CurrentMenu := DebugPlayerMenu;
    1: CurrentMenu := DebugCreaturesMenu;
    2: CurrentMenu := DebugItemsMenu;
    3: CurrentMenu := DebugLevelMenu;
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
         EditLevelLightsMenu := TEditLevelLightsMenu.Create;
         CurrentMenu := EditLevelLightsMenu;
       end;
    10:ForceThunder;
    11:UserQuit := true;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TDebugPlayerMenu ----------------------------------------------------------- }

constructor TDebugPlayerMenu.Create;
begin
  inherited Create;

  ViewAngleSlider := TViewAngleSlider.Create;

  { Note that Player is not created at this point.
    We will init Value of these sliders later. }
  RotationHorizontalSpeedSlider := TGLMenuFloatSlider.Create(0.5, 10, 1);
  RotationVerticalSpeedSlider := TGLMenuFloatSlider.Create(0.5, 10, 1);
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

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugPlayerMenu.CurrentItemSelected;

  procedure PlayerSetMaxLife;
  var
    Value: Single;
  begin
    Value := Player.MaxLife;
    if MessageInputQuery(Glw, 'Set Player.MaxLife',
      Value, taLeft) then
      Player.MaxLife := Value;
  end;

  procedure PlayerMaxLife;
  begin
    if Player.Dead then
      MessageOK(Glw, 'No can do. You are dead.', taLeft) else
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
    8: CurrentMenu := DebugMenu;
  end;
end;

procedure TDebugPlayerMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    2: begin
         ViewAngleDegX := ViewAngleSlider.Value;
         { After changing ViewAngleDegX, game's OnResize must be called. }
         Glw.EventResize;
       end;
    3: Player.Navigator.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
    4: Player.Navigator.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
    5: Player.Navigator.CameraDir := VectorAdjustToLength(
         Player.Navigator.CameraDir, PlayerSpeedSlider.Value);
  end;
end;

{ TDebugCreaturesMenu -------------------------------------------------------- }

constructor TDebugCreaturesMenu.Create;
begin
  inherited Create;

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

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugCreaturesMenu.CurrentItemSelected;

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
      ResultIndex := ChooseByMenu(DrawUnderMenu, S);
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

      MessageOK(Glw, S, taLeft);
    finally S.Free end;
  end;

  procedure KillAll;
  var
    I: Integer;
  begin
    for I := 0 to Level.Creatures.High do
      if Level.Creatures[I].Life > 0 then
      begin
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
      Position := VectorAdd(Player.Navigator.CameraPos,
        VectorAdjustToLength(Player.Navigator.CameraDir, DirectionAttenuation));
      Direction := Player.Navigator.CameraDir;

      Level.Creatures.Add(
        Kind.CreateDefaultCreature(Position, Direction, Level.AnimationTime,
          Kind.DefaultMaxLife));

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
        MessageOK(Glw, Format('Creature "%s" is not used by anything, ' +
          'cannot reload',  [Kind.VRMLNodeName])) else
        Kind.RedoPrepareRender;
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
    8: CurrentMenu := DebugMenu;
  end;
end;

{ TDebugLevelMenu -------------------------------------------------------- }

constructor TDebugLevelMenu.Create;
begin
  inherited Create;

  LevelOctreeMaxDepthSlider := TGLMenuIntegerSlider.Create(
    -1, 40, DebugLevelOctreeMaxDepth);
  LevelOctreeMaxLeafItemsCountSlider := TGLMenuIntegerSlider.Create(
    -1, 40, DebugLevelOctreeMaxLeafItemsCount);

  Items.Add('Change to level');
  Items.Add('Restart current level (preserving camera)');
  Items.AddObject('Future level octree max depth',
    LevelOctreeMaxDepthSlider);
  Items.AddObject('Future level octree max leaf items count',
    LevelOctreeMaxLeafItemsCountSlider);
  Items.Add('Back');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugLevelMenu.CurrentItemSelected;

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

      Index := ChooseByMenu(DrawUnderMenu, S);

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
    Pos := Player.Navigator.CameraPos;
    Dir := Player.Navigator.CameraDir;
    Up := Player.Navigator.CameraUp;

    LevelFinished(Level.Name);
    LevelFinishedFlush;

    Player.Navigator.CameraPos := Pos;
    Player.Navigator.CameraDir := Dir;
    Player.Navigator.CameraUp := Up;

    UserQuit := true;
  end;

begin
  inherited;

  case CurrentItem of
    0: ChangeToLevel;
    1: RestartLevel;
    2, 3: ;
    4: CurrentMenu := DebugMenu;
  end;
end;

procedure TDebugLevelMenu.CurrentItemAccessoryValueChanged;
begin
  inherited;

  case CurrentItem of
    2: DebugLevelOctreeMaxDepth := LevelOctreeMaxDepthSlider.Value;
    3: DebugLevelOctreeMaxLeafItemsCount :=
         LevelOctreeMaxLeafItemsCountSlider.Value;
  end;
end;

{ TDebugItemsMenu ------------------------------------------------------------ }

constructor TDebugItemsMenu.Create;
begin
  inherited Create;

  Items.Add('Give me 20 instances of every possible item');
  Items.Add('Reload items/kinds.xml file');
  Items.Add('Reload animations/models of specific item');
  Items.Add('Back');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugItemsMenu.CurrentItemSelected;

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
      ResultIndex := ChooseByMenu(DrawUnderMenu, S);
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
      Kind.RedoPrepareRender;
  end;

begin
  inherited;

  case CurrentItem of
    0: GiveItems;
    1: ItemsKinds.LoadFromFile;
    2: ReloadItemAnimation;
    3: CurrentMenu := DebugMenu;
  end;
end;

{ TEditHeadlightMenu --------------------------------------------------------- }

constructor TEditHeadlightMenu.Create;
begin
  inherited Create;

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  AmbientIntensitySlider := TGLMenuFloatSlider.Create(0, 1, Level.Headlight.AmbientIntensity);

  ColorSlider[0] := TGLMenuFloatSlider.Create(0, 1, Level.Headlight.Color[0]);
  ColorSlider[1] := TGLMenuFloatSlider.Create(0, 1, Level.Headlight.Color[1]);
  ColorSlider[2] := TGLMenuFloatSlider.Create(0, 1, Level.Headlight.Color[2]);

  IntensitySlider := TGLMenuFloatSlider.Create(0, 1, Level.Headlight.Intensity);

  SpotArgument := TGLMenuBooleanArgument.Create(Level.Headlight.Spot);

  Items.AddObject('Ambient intensity'  , AmbientIntensitySlider);

  Items.AddObject('Color red'  , ColorSlider[0]);
  Items.AddObject('Color green', ColorSlider[1]);
  Items.AddObject('Color blue' , ColorSlider[2]);

  Items.AddObject('Intensity'  , IntensitySlider);

  Items.Add('Change attenuation');

  Items.AddObject('Spot', SpotArgument);

  Items.Add('Change spot properties');

  Items.Add('Back');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TEditHeadlightMenu.CurrentItemSelected;

  procedure ChangeAttenuation;
  var
    Vector3: TVector3Single;
  begin
    Vector3 := Level.Headlight.Attenuation;
    if MessageInputQueryVector3Single(Glw, 'Change headlight Attenuation',
      Vector3, taLeft) then
      Level.Headlight.Attenuation := Vector3;
  end;

  procedure ChangeSpotProperties;
  var
    Value: Single;
  begin
    Value := Level.Headlight.SpotCutOffAngle;
    if MessageInputQuery(Glw, 'Change headlight SpotCutOffAngle',
      Value, taLeft) then
      Level.Headlight.SpotCutOffAngle := Value;

    Value := Level.Headlight.SpotDropOffRate;
    if MessageInputQuery(Glw, 'Change headlight SpotDropOffRate',
      Value, taLeft) then
      Level.Headlight.SpotDropOffRate := Value;
  end;

begin
  case CurrentItem of
    0..4: Exit;
    5: begin
         ChangeAttenuation;
         Level.Headlight.Render(0, false { it should be already enabled });
       end;
    6: begin
         SpotArgument.Value := not SpotArgument.Value;
         Level.Headlight.Spot := SpotArgument.Value;
         Level.Headlight.Render(0, false { it should be already enabled });
       end;
    7: begin
         ChangeSpotProperties;
         Level.Headlight.Render(0, false { it should be already enabled });
       end;
    8: CurrentMenu := EditLevelLightsMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TEditHeadlightMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    0:
      begin
        Level.Headlight.AmbientIntensity := AmbientIntensitySlider.Value;
        Level.Headlight.Render(0, false { it should be already enabled });
      end;
    1..3:
      begin
        Level.Headlight.Color[CurrentItem-1] := ColorSlider[CurrentItem-1].Value;
        Level.Headlight.Render(0, false { it should be already enabled });
      end;
    4:
      begin
        Level.Headlight.Intensity := IntensitySlider.Value;
        Level.Headlight.Render(0, false { it should be already enabled });
      end;
    else Exit;
  end;
end;

{ TEditLevelLightsMenu ------------------------------------------------------- }

constructor TEditLevelLightsMenu.Create;
var
  I: Integer;
  LightNode: TNodeGeneralLight;
begin
  inherited Create;

  { To better visualize changes to Level.GlobalAmbientLight }
  DrawBackgroundRectangle := false;

  AmbientColorSlider[0] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[0]);
  AmbientColorSlider[1] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[1]);
  AmbientColorSlider[2] := TGLMenuFloatSlider.Create(0, 1, Level.GlobalAmbientLight[2]);

  for I := 0 to Level.LightSet.Lights.High do
  begin
    LightNode := Level.LightSet.Lights.Items[I].LightNode;
    Items.Add(Format('Edit %d: %s "%s"',
      [I, LightNode.NodeTypeName, LightNode.NodeName]));
  end;
  Items.Add('Output level lights on console');
  Items.Add('Save level lights to level xxx_lights.wrl file');
  Items.AddObject('Global ambient light red'  , AmbientColorSlider[0]);
  Items.AddObject('Global ambient light green', AmbientColorSlider[1]);
  Items.AddObject('Global ambient light blue' , AmbientColorSlider[2]);
  Items.Add('Edit headlight');
  Items.Add('Back to debug menu');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TEditLevelLightsMenu.CurrentItemSelected;

  function LightSetVRMLComment: string;
  begin
    Result := Format('Generated by "The Castle" lights editor for level "%s"',
      [Level.Title]);
  end;

begin
  case CurrentItem - Level.LightSet.Lights.Count of
    0: begin
         if StdOutStream <> nil then
           SaveToVRMLFile(Level.LightSet.RootNode, StdOutStream, LightSetVRMLComment) else
           MessageOK(Glw, 'No stdout available. On Windows you must run the game ' +
             'from the command-line to get stdout.', taLeft);
       end;
    1: begin
         if MessageYesNo(Glw, Format('This will permanently overwrite file "%s". ' +
           'Are you sure you want to save the lights ?',
           [Level.LightSetFileName]), taLeft) then
           SaveToVRMLFile(Level.LightSet.RootNode, Level.LightSetFileName,
             LightSetVRMLComment);
       end;
    2, 3, 4: ;
    5: begin
         if Level.Headlight <> nil then
         begin
           FreeAndNil(EditHeadlightMenu);
           EditHeadlightMenu := TEditHeadlightMenu.Create;
           CurrentMenu := EditHeadlightMenu;
         end else
           MessageOK(Glw, 'No headlight in level ' +
             ' (set NavigationInfo.headlight to TRUE to get headlight)', taLeft);
       end;
    6: CurrentMenu := DebugMenu;
    else
       begin
         FreeAndNil(EditOneLightMenu);
         EditOneLightMenu := TEditOneLightMenu.Create(
           Level.LightSet.Lights.Items[CurrentItem].LightNode);
         CurrentMenu := EditOneLightMenu;
       end;
  end;
end;

procedure TEditLevelLightsMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem - Level.LightSet.Lights.Count of
    2: Level.GlobalAmbientLight[0] := AmbientColorSlider[0].Value;
    3: Level.GlobalAmbientLight[1] := AmbientColorSlider[1].Value;
    4: Level.GlobalAmbientLight[2] := AmbientColorSlider[2].Value;
    else Exit;
  end;

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Level.GlobalAmbientLight);
end;

{ TEditOneLightMenu ---------------------------------------------------------- }

constructor TEditOneLightMenu.Create(ALight: TNodeGeneralLight);
begin
  inherited Create;

  { To better visualize light changes. }
  DrawBackgroundRectangle := false;

  Light := ALight;

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

  Items.AddObject('Red color', RedColorSlider);
  Items.AddObject('Green color', GreenColorSlider);
  Items.AddObject('Blue color', BlueColorSlider);
  Items.AddObject('Intensity', IntensitySlider);
  Items.AddObject('Ambient intensity', AmbientIntensitySlider);
  Items.AddObject('On', OnArgument);
  Items.AddObject('Shadows', ShadowsArgument);
  Items.AddObject('Shadows main light', ShadowsMainArgument);
  Items.Add('Point/SpotLight: Change location');
  Items.Add('Point/SpotLight: Change attenuation');
  Items.Add('DirectionalLight: Change direction');
  Items.Add('SpotLight: Change direction');
  Items.Add('SpotLight: Change beamWidth/dropOffRate');
  Items.Add('SpotLight: Change cutOffAngle');
  Items.Add('Back');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TEditOneLightMenu.CurrentItemSelected;

  function MessageInputQueryVector3SingleP(
    glwin: TGLWindow; const Title: string;
    var Value: TVector3Single; TextAlign: TTextAlign;
    const OnP: TVector3Single): boolean;
  var s: string;
  begin
   Result := false;
   s := Format('%g %g %g', [Value[0], Value[1], Value[2]]);
   if MessageInputQuery(glwin, Title, s, TextAlign) then
   begin
    try
     if LowerCase(Trim(S)) = 'p' then
       Value := OnP else
       Value := Vector3SingleFromStr(s);
     Result := true;
    except
     on E: EConvertError do
     begin
      MessageOK(glwin, 'Invalid vector 3 value : ' + E.Message, taLeft);
     end;
    end;
   end;
  end;

var
  Vector: TVector3Single;
  Value: Single;
begin
  case CurrentItem of
    0, 1, 2, 3, 4: ;
    5: begin
         OnArgument.Value := not OnArgument.Value;
         Light.FdOn.Value := OnArgument.Value;
         Level.LightSet.CalculateLights;
       end;
    6: begin
         ShadowsArgument.Value := not ShadowsArgument.Value;
         Light.FdKambiShadows.Value := ShadowsArgument.Value;
         Level.LightSet.CalculateLights;
       end;
    7: begin
         ShadowsMainArgument.Value := not ShadowsMainArgument.Value;
         Light.FdKambiShadowsMain.Value := ShadowsMainArgument.Value;
         Level.LightSet.CalculateLights;
       end;
    8: begin
         if Light is TNodeGeneralPositionalLight then
         begin
           Vector := TNodeGeneralPositionalLight(Light).FdLocation.Value;
           if MessageInputQueryVector3SingleP(Glw, 'Change location' +nl+
             '(Input "P" to use current player''s location)',
             Vector, taLeft, Player.Navigator.CameraPos) then
           begin
             TNodeGeneralPositionalLight(Light).FdLocation.Value := Vector;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    9: begin
         if Light is TNodeGeneralPositionalLight then
         begin
           Vector := TNodeGeneralPositionalLight(Light).FdAttenuation.Value;
           if MessageInputQueryVector3Single(Glw, 'Change attenuation',
             Vector, taLeft) then
           begin
             TNodeGeneralPositionalLight(Light).FdAttenuation.Value := Vector;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    10:begin
         if Light is TNodeGeneralDirectionalLight then
         begin
           Vector := TNodeGeneralDirectionalLight(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Glw, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Navigator.CameraDir) then
           begin
             TNodeGeneralDirectionalLight(Light).FdDirection.Value := Vector;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    11:begin
         if Light is TNodeSpotLight_1 then
         begin
           Vector := TNodeSpotLight_1(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Glw, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Navigator.CameraDir) then
           begin
             TNodeSpotLight_1(Light).FdDirection.Value := Vector;
             Level.LightSet.CalculateLights;
           end;
         end else
         if Light is TNodeSpotLight_2 then
         begin
           Vector := TNodeSpotLight_2(Light).FdDirection.Value;
           if MessageInputQueryVector3SingleP(Glw, 'Change direction' +nl+
             '(Input "P" to use current player''s direction)',
             Vector, taLeft, Player.Navigator.CameraDir) then
           begin
             TNodeSpotLight_2(Light).FdDirection.Value := Vector;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    12:begin
         if Light is TNodeSpotLight_1 then
         begin
           Value := TNodeSpotLight_1(Light).FdDropOffRate.Value;
           if MessageInputQuery(Glw, 'Change dropOffRate', Value, taLeft) then
           begin
             TNodeSpotLight_1(Light).FdDropOffRate.Value := Value;
             Level.LightSet.CalculateLights;
           end;
         end else
         if Light is TNodeSpotLight_2 then
         begin
           Value := TNodeSpotLight_2(Light).FdBeamWidth.Value;
           if MessageInputQuery(Glw, 'Change beamWidth', Value, taLeft) then
           begin
             TNodeSpotLight_2(Light).FdBeamWidth.Value := Value;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    13:begin
         if Light is TNodeSpotLight_1 then
         begin
           Value := TNodeSpotLight_1(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Glw, 'Change cutOffAngle', Value, taLeft) then
           begin
             TNodeSpotLight_1(Light).FdCutOffAngle.Value := Value;
             Level.LightSet.CalculateLights;
           end;
         end else
         if Light is TNodeSpotLight_2 then
         begin
           Value := TNodeSpotLight_2(Light).FdCutOffAngle.Value;
           if MessageInputQuery(Glw, 'Change cutOffAngle', Value, taLeft) then
           begin
             TNodeSpotLight_2(Light).FdCutOffAngle.Value := Value;
             Level.LightSet.CalculateLights;
           end;
         end;
       end;
    14:CurrentMenu := EditLevelLightsMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TEditOneLightMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    0: Light.FdColor.Value[0] := RedColorSlider.Value;
    1: Light.FdColor.Value[1] := GreenColorSlider.Value;
    2: Light.FdColor.Value[2] := BlueColorSlider.Value;
    3: Light.FdIntensity.Value := IntensitySlider.Value;
    4: Light.FdAmbientIntensity.Value := AmbientIntensitySlider.Value;
    else Exit;
  end;

  Level.LightSet.CalculateLights;
end;

{ global things -------------------------------------------------------------- }

{$I castlemenucallbacks.inc}

procedure Draw2d(Draw2DData: Pointer);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  CurrentMenu.Draw;
end;

procedure Draw(Glwin: TGLWindow);
begin
  DrawUnderMenu(Glwin);

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glProjectionPushPopOrtho2D(@Draw2d, nil,
      0, Glwin.Width, 0, Glwin.Height);
  glPopAttrib;
end;

procedure ShowDebugMenu(ADrawUnderMenu: TDrawFunc);
var
  SavedMode: TGLMode;
begin
  DrawUnderMenu := ADrawUnderMenu;

  DebugPlayerMenu.RotationHorizontalSpeedSlider.Value :=
    Player.Navigator.RotationHorizontalSpeed;
  DebugPlayerMenu.RotationVerticalSpeedSlider.Value :=
    Player.Navigator.RotationVerticalSpeed;
  DebugPlayerMenu.PlayerSpeedSlider.Value :=
    VectorLen(Player.Navigator.CameraDir);

  SavedMode := TGLMode.Create(Glw, 0, true);
  try
    { This is needed, because when changing ViewAngleDegX we will call
      Glw.OnResize to set new projection matrix, and this
      new projection matrix should stay for the game. }
    SavedMode.RestoreProjectionMatrix := false;

    SetStandardGLWindowState(Glw, @Draw, @CloseQuery, Glw.OnResize,
      nil, false, true { FPSActive should not be needed anymore, but I leave it. },
      false, K_None, #0, false, false);

    { Otherwise messages don't look good, because the text is mixed
      with the menu text. }
    GLWinMessagesTheme.RectColor[3] := 1.0;

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnMouseUp := @MouseUp;
    Glw.OnMouseMove := @MouseMove;
    Glw.OnIdle := @Idle;

    CurrentMenu := DebugMenu;
    UserQuit := false;

    repeat
      Glwm.ProcessMessage(true);
    until GameEnded or UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  DebugMenu := TDebugMenu.Create;
  DebugPlayerMenu := TDebugPlayerMenu.Create;
  DebugCreaturesMenu := TDebugCreaturesMenu.Create;
  DebugLevelMenu := TDebugLevelMenu.Create;
  DebugItemsMenu := TDebugItemsMenu.Create;
  CurrentMenu := DebugMenu;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil;
  FreeAndNil(DebugMenu);
  FreeAndNil(EditLevelLightsMenu);
  FreeAndNil(EditOneLightMenu);
  FreeAndNil(EditHeadlightMenu);
  FreeAndNil(DebugItemsMenu);
  FreeAndNil(DebugCreaturesMenu);
  FreeAndNil(DebugLevelMenu);
  FreeAndNil(DebugPlayerMenu);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.