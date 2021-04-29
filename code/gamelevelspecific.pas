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

{ Level logic. TLevelLogic descendants specialized for castle1 levels. }
unit GameLevelSpecific;

{$I castlegameconf.inc}

interface

uses DOM,
  CastleScene, CastleBoxes, CastleVectors, CastleShapes, CastlePlayer,
  CastleLevels, X3DNodes, X3DFields, X3DTIme, CastleTransform, CastleSoundEngine,
  GameCreatures, CastleCreatures, Classes, CastleTimeUtils, CastleColors,
  CastleFrustum, CastleTransformExtra, CastleResources, CastleRenderOptions,
  GameSound;

type
  { Level that may have a boss life indicator. }
  TBossLevel = class(TLevelLogic)
  public
    { What to show on boss creature indicator. }
    function BossIndicator(out Life, MaxLife: Single): boolean; virtual;
  end;

  TCastleHallLevel = class(TBossLevel)
  private
    const
      WerewolvesCount = 4;
      WerewolfFirstLight = 1;
    var
      Symbol: TCastleScene;
      Button: TCastleScene;
      StairsBlocker: TCastleScene;
      StairsBlockerMiddle: TVector3;
      FLevelExitBox: TBox3D;
      WerewolfAppearPosition: array [0..WerewolvesCount - 1] of TVector3;
      WerewolfAppeared: boolean;
      WerewolfCreature: array [0..WerewolvesCount - 1] of TWerewolfCreature;
  protected
    function Placeholder(const Shape: TShape; const PlaceholderName: string): boolean; override;
    procedure ButtonAnimationIsActiveChanged(
      Event: TX3DEvent; Value: TX3DField; const ATime: TX3DTime);
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
    function BossIndicator(out Life, MaxLife: Single): boolean; override;
  end;

  TGateLevel = class(TLevelLogic)
  private
    FGateExitBox: TBox3D;

    Teleport: TCastleScene;
    Teleport1, Teleport2: TCastleTransform;
    Teleport1Box, Teleport2Box: TBox3D;
    Teleport1Destination: TVector3;
    Teleport2Destination: TVector3;

    SacrilegeAmbushStartingPosition: array [0..5] of TVector3;
    SwordAmbushStartingPosition: array [0..2] of TVector3;

    SacrilegeAmbushDone: boolean;
    SwordAmbushDone: boolean;

    FSacrilegeBox: TBox3D;

    CartLastSoundTime: Single;
    CartSoundPosition: TVector3;
  protected
    function Placeholder(const Shape: TShape; const PlaceholderName: string): boolean; override;
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TTowerLevel = class(TLevelLogic)
  private
    MovingElevator: TCastleLinearMoving;
    Elevator: TCastleScene;
    ElevatorButton: TCastleScene;
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
  end;

  TGameWinAnimation = (
    gwaNone,
    gwaAnimateTo1,
    gwaAnimateTo2,
    gwaFinished);

  TCagesLevel = class(TBossLevel)
  private
    SpidersAppearing: TCastleTransform;
    NextSpidersAppearingTime: Single;
    { Instance of boss creature, if any, on the level. @nil if no boss creature
      exists on this level. }
    Boss: TCreature;
    HintOpenDoorScript: TX3DNode;
    FGateExit: TCastleScene;
    FGameWinAnimation: TGameWinAnimation;
    FEndSequence: TCastleScene;
    procedure SetGameWinAnimation(Value: TGameWinAnimation);
  protected
    procedure PlaceholdersEnd; override;
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;

    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
    function BossIndicator(out Life, MaxLife: Single): boolean; override;

    { Stage of game win animation. Known by this level, to display appropriate
      geometry and background. }
    property GameWinAnimation: TGameWinAnimation
      read FGameWinAnimation write SetGameWinAnimation default gwaNone;
  end;

  TDoomLevelDoor = class(TCastleLinearMoving)
  public
    StayOpenTime: Single;

    constructor Create(AOwner: TComponent); override;

    procedure BeforeTimeIncrease(const NewTime: TFloatTime); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;

    property Pushes default false;

    { No way to express this:
    property SoundGoBeginPosition default stDoorClose;
    property SoundGoEndPosition default stDoorOpen;
    }

    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

  TDoomE1M1Level = class(TLevelLogic)
  private
    FakeWall: TCastleScene;

    MovingElevator49: TCastleLinearMoving;
    Elevator49: TCastleScene;
    Elevator49DownBox: TBox3D;

    MovingElevator9a9b: TCastleLinearMoving;
    Elevator9a9b: TCastleScene;

    ExitButton: TCastleScene;
  protected
    function Placeholder(const Shape: TShape; const PlaceholderName: string): boolean; override;
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TGateBackgroundLevel = class(TLevelLogic)
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
  end;

  TFountainLevel = class(TLevelLogic)
  public
    constructor Create(const AOwner: TComponent;
      const ALevelProperties: TLevelProperties;
      const MainScene: TCastleScene; const DOMElement: TDOMElement); override;
    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

implementation

uses SysUtils, Math,
  CastleFilesUtils, CastleUtils,
  CastleGLUtils, CastleStringUtils, CastleMessages,
  GamePlay, CastleGameNotifications, CastleInputs, CastleGL,
  GameWindow, GameX3DProcessing,
  GameAnimationTricks, GameVideoOptions, CastleSceneCore, CastleProgress,
  CastleXMLUtils, GameItems;

function LevelsPath: string;
begin
  Result := 'castle-data:/levels/';
end;

procedure NotificationInteractFailed(const S: string);
begin
  Notifications.Show(S);
  SoundEngine.Sound(stPlayerInteractFailed);
end;

{ TBossLevel ----------------------------------------------------------------- }

function TBossLevel.BossIndicator(out Life, MaxLife: Single): boolean;
begin
  Result := false;
end;

{ TStairsBlocker ------------------------------------------------------------- }

type
  TStairsBlocker = class(TCastleScene)
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

function TStairsBlocker.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  NotificationInteractFailed('You are not able to open it');
end;

{ TCastleHallButton ---------------------------------------------------------- }

type
  TCastleHallButton = class(TCastleScene)
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

function TCastleHallButton.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  if Distance < 10.0 then
  begin
    if CurrentAnimation <> nil then
      NotificationInteractFailed('Button is already pressed') else
    begin
      PlayAnimation('animation', false);
      Notifications.Show('You press the button');
    end;
  end else
    NotificationInteractFailed('You see a button. You cannot reach it from here');
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  CastleHallLevelPath: string;
begin
  inherited;

  CastleHallLevelPath := LevelsPath + 'castle_hall/';

  { Do not build collisions structure, let it collide as a bounding box.
    Doing it properly (with PrepareForCollisions = true) would require
    reexporting as castle-anim-frames (with it's bbox recorded). }
  Symbol := TCastleScene.Create(Self);
  Symbol.Load(CastleHallLevelPath + 'symbol.kanim');
  Symbol.ProcessEvents := true;
  Symbol.CastShadowVolumes := false; { shadow would not be visible anyway }
  Level.RootTransform.Add(Symbol);

  { Do not build collisions structure, let it collide as a bounding box.
    Doing it properly (with PrepareForCollisions = true) would require
    reexporting as castle-anim-frames (with it's bbox recorded). }
  Button := TCastleHallButton.Create(Self);
  Button.Load(CastleHallLevelPath + 'button.kanim');
  Button.ProcessEvents := true;
  Button.CastShadowVolumes := false; { strange ghost shadow on symbol would be visible }
  Button.AnimationTimeSensor('animation').EventIsActive.AddNotification(
    @ButtonAnimationIsActiveChanged);

  Level.RootTransform.Add(Button);

  StairsBlocker := TStairsBlocker.Create(Self);
  StairsBlocker.Load(CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl');
  StairsBlocker.ProcessEvents := true;
  StairsBlocker.Spatial := [ssDynamicCollisions];
  StairsBlocker.CastShadowVolumes := false; { shadow would not be visible anyway }
  Level.RootTransform.Add(StairsBlocker);

  { get StairsBlocker.BoundingBox.Center when it GetExists.
    Later StairsBlocker will have Exists = false, so bbox will be empty,
    but we'll need StairsBlockerMiddle position. }
  StairsBlockerMiddle := StairsBlocker.BoundingBox.Center;
end;

function BoxDownPosition(const Box: TBox3D): TVector3;
begin
  Result[0] := (Box.Data[0].Data[0] + Box.Data[1].Data[0]) / 2;
  Result[1] := (Box.Data[0].Data[1] + Box.Data[1].Data[1]) / 2;
  Result[2] :=  Box.Data[0].Data[2];
end;

function TCastleHallLevel.Placeholder(const Shape: TShape;
  const PlaceholderName: string): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if PlaceholderName = 'LevelExitBox' then
  begin
    FLevelExitBox := Shape.BoundingBox;
    Exit(true);
  end;

  for I := 0 to WerewolvesCount - 1 do
    if PlaceholderName = 'WerewolfAppear_' + IntToStr(I) then
    begin
      WerewolfAppearPosition[I] := BoxDownPosition(Shape.BoundingBox);
      Exit(true);
    end;
end;

procedure TCastleHallLevel.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  WerewolfAliveCount: Cardinal;

  procedure DestroyStairsBlocker;
  begin
    if StairsBlocker.Exists then
    begin
      SoundEngine.Sound3d(stStairsBlockerDestroyed, StairsBlockerMiddle);
      StairsBlocker.Exists := false;
    end;
  end;

  procedure WerewolfShowLights;
  var
    I: Integer;
    LightNode: TAbstractPositionalLightNode;
  begin
    if WerewolfAliveCount = 0 then
    begin
      { turn light over stairs to next level }
      LightNode := SceneManager.Items.MainScene.GlobalLights.Items[WerewolfFirstLight].Node as
        TAbstractPositionalLightNode;
      LightNode.Location := StairsBlockerMiddle;
      LightNode.IsOn := true;

      for I := 1 to WerewolvesCount - 1 do
      begin
        LightNode := SceneManager.Items.MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
          TAbstractPositionalLightNode;
        LightNode.IsOn := false;
      end;
    end else
    begin
      { turn light for each alive werewolf }
      for I := 0 to WerewolvesCount - 1 do
      begin
        LightNode := SceneManager.Items.MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
          TAbstractPositionalLightNode;
        LightNode.IsOn := not WerewolfCreature[I].Dead;
        LightNode.Location := WerewolfCreature[I].Middle;
      end;
    end;
  end;

  function GetWerewolfAliveCount: Cardinal;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to WerewolvesCount - 1 do
      if not WerewolfCreature[I].Dead then
        Inc(Result);
  end;

begin
  inherited;

  if (Player = nil) then Exit;

  if FLevelExitBox.Contains(Player.Translation) then
  begin
    LevelFinished('cages');
  end;

  if WerewolfAppeared then
  begin
    WerewolfAliveCount := GetWerewolfAliveCount;
    WerewolfShowLights;
    if WerewolfAliveCount = 0 then
      DestroyStairsBlocker;
  end;
end;

procedure TCastleHallLevel.ButtonAnimationIsActiveChanged(
  Event: TX3DEvent; Value: TX3DField; const ATime: TX3DTime);

  procedure WerewolfAppear;
  var
    I: Integer;
    LightNode: TAbstractPositionalLightNode;
    ShadowLight: PLightInstance;
  begin
    Assert(not WerewolfAppeared);

    for I := 0 to WerewolvesCount - 1 do
      WerewolfCreature[I] := Werewolf.CreateCreature(SceneManager.LevelProperties,
        WerewolfAppearPosition[I],
        Player.Translation - WerewolfAppearPosition[I])
        as TWerewolfCreature;

    WerewolfAppeared := true;

    WerewolfCreature[0].Howl(true);

    { change the lights }
    SceneManager.Items.HeadlightNode.AmbientIntensity := 0.8;
    SceneManager.Items.HeadlightNode.Color := Vector3(1, 0, 0);
    SceneManager.Items.HeadlightNode.Intensity := 0.2;

    for I := 0 to WerewolvesCount - 1 do
    begin
      LightNode := SceneManager.Items.MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
        TAbstractPositionalLightNode;
      LightNode.Color := Vector3(1, 0, 0);
      LightNode.Attenuation := Vector3(1, 0.1, 0);
      LightNode.ShadowVolumes := true;
    end;

    ShadowLight := SceneManager.Items.MainScene.GlobalLights.FindName('FakeShadowPosition');
    Check(ShadowLight <> nil, 'FakeShadowPosition light not found on castle_hall level');
    ShadowLight^.Node.ShadowVolumes := true;
    (ShadowLight^.Node as TAbstractPunctualLightNode).ShadowVolumesMain := true;
  end;

begin
  if not (Value as TSFBool).Value then // if isActive changed to false...
    if Symbol.CurrentAnimation = nil then
    begin
      Symbol.PlayAnimation('animation', false);
      Symbol.Collides := false;
      SoundEngine.Sound3d(stCastleHallSymbolMoving, Vector3(0, 0, 0));

      WerewolfAppear;
    end;
end;

procedure TCastleHallLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword. Otherwise player would start the level
    without any weapon, and there's no weapon to be found on
    the level... }
  NewPlayer.PickItem(Sword.CreateItem(1));
end;

function TCastleHallLevel.BossIndicator(
  out Life, MaxLife: Single): boolean;
var
  AliveCount: Cardinal;
  I: Integer;
begin
  Result := WerewolfAppeared;
  if Result then
  begin
    Life := 0;
    MaxLife := 0;
    AliveCount := 0;
    for I := 0 to WerewolvesCount - 1 do
    begin
      MaxLife += WerewolfCreature[I].MaxLife;
      if not WerewolfCreature[I].Dead then
      begin
        Inc(AliveCount);
        Life += WerewolfCreature[I].Life;
      end;
    end;
    Result := AliveCount <> 0;
  end;
end;

{ TGateLevel ----------------------------------------------------------------- }

constructor TGateLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  Cart: TCastleScene;
  GateLevelPath: string;
begin
  inherited;

  GateLevelPath := LevelsPath + 'gate/';

  Teleport := TCastleScene.Create(Self);
  Teleport.Load(GateLevelPath + 'teleport.wrl');
  Teleport.ProcessEvents := true;
  Teleport.Collides := false;

  Teleport1 := TCastleTransform.Create(Self);
  { set rotation axis. Rotation angle will be increased in each Update }
  Teleport1.Rotation :=  Vector4(1, 1, 0, 0);
  Teleport1.Add(Teleport);
  Level.RootTransform.Add(Teleport1);

  Teleport2 := TCastleTransform.Create(Self);
  Teleport2.Rotation :=  Vector4(1, 1, 0, 0);
  Teleport2.Add(Teleport);
  Level.RootTransform.Add(Teleport2);

  { Do not build collisions structure, let it collide as a bounding box.
    Doing it properly (with PrepareForCollisions = true) would require
    reexporting as castle-anim-frames (with it's bbox recorded). }
  Cart := TCastleScene.Create(Self);
  Cart.Load(GateLevelPath + 'cart.kanim');
  Cart.ProcessEvents := true;
  Cart.PlayAnimation('animation', true);
  Level.RootTransform.Add(Cart);

  CartSoundPosition := Cart.BoundingBox.Center;

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
end;

function TGateLevel.Placeholder(const Shape: TShape; const PlaceholderName: string): boolean;
var
  I: Integer;
begin
  Result := inherited;
  if Result then Exit;

  if PlaceholderName = 'GateExitBox' then
  begin
    FGateExitBox := Shape.BoundingBox;
    Exit(true);
  end;

  if PlaceholderName = 'Teleport1Box' then
  begin
    Teleport1Box := Shape.BoundingBox;
    Teleport1.Translation := Teleport1Box.Center;
    Teleport2Destination := Teleport1Box.Center;
    Teleport2Destination.Data[0] -= 2;
    Teleport2Destination.Data[1] -= 2;
    Exit(true);
  end;

  if PlaceholderName = 'Teleport2Box' then
  begin
    Teleport2Box := Shape.BoundingBox;
    Teleport2.Translation := Teleport2Box.Center;
    Teleport1Destination := Teleport2Box.Center;
    Teleport1Destination.Data[0] += 2;
    Teleport1Destination.Data[1] += 2;
    Exit(true);
  end;

  if PlaceholderName = 'SacrilegeBox' then
  begin
    FSacrilegeBox := Shape.BoundingBox;
    Exit(true);
  end;

  for I := 0 to High(SacrilegeAmbushStartingPosition) do
    if PlaceholderName = 'SacrilegeGhost_' + IntToStr(I) then
    begin
      SacrilegeAmbushStartingPosition[I] := BoxDownPosition(Shape.BoundingBox);
      Exit(true);
    end;

  for I := 0 to High(SwordAmbushStartingPosition) do
    if PlaceholderName = 'SwordGhost_' + IntToStr(I) then
    begin
      SwordAmbushStartingPosition[I] := BoxDownPosition(Shape.BoundingBox);
      Exit(true);
    end;
end;

procedure TGateLevel.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);

  procedure RejectGateExitBox;
  var
    NewPosition: TVector3;
  begin
    NewPosition := Player.Translation;
    { Although I do him knockback, I also change the position
      to make sure that he is thrown outside of FGateExitBox. }
    NewPosition.Data[1] := FGateExitBox.Data[0].Data[1] - 0.1;
    Player.Translation := NewPosition;

    GamePlay.Player.Hurt(0, Vector3(0, -1, 0), 2, nil);
  end;

  procedure TeleportWork(Teleport: TCastleTransform; const TeleportBox: TBox3D;
    const Destination: TVector3);
  var
    Rot: TVector4;
  begin
    Rot := Teleport.Rotation;
    Rot.Data[3] += 0.175 * SecondsPassed;
    Teleport.Rotation := Rot;

    if TeleportBox.Contains(Player.Translation) then
    begin
      Player.Translation := Destination;
      GamePlay.Player.WalkNavigation.CancelFalling;

      SceneManager.Items.MainScene.ViewChangedSuddenly;

      SoundEngine.Sound(stTeleport);
    end;
  end;

  procedure SacrilegeAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3;
  begin
    SoundEngine.Sound(stSacrilegeAmbush);
    for I := 0 to High(SacrilegeAmbushStartingPosition) do
    begin
      CreaturePosition := SacrilegeAmbushStartingPosition[I];
      CreatureDirection := Player.Translation - CreaturePosition;
      Ghost.CreateCreature(SceneManager.LevelProperties, CreaturePosition, CreatureDirection);
    end;
  end;

  procedure SwordAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3;
  begin
    for I := 0 to High(SwordAmbushStartingPosition) do
    begin
      CreaturePosition := SwordAmbushStartingPosition[I];
      CreatureDirection := Player.Translation - CreaturePosition;
      Ghost.CreateCreature(SceneManager.LevelProperties, CreaturePosition, CreatureDirection);
    end;
  end;

const
  { In seconds. }
  CartSoundRepeatTime = 10.0;
begin
  inherited;

  if (Player = nil) then Exit;

  if FGateExitBox.Contains(Player.Translation) then
  begin
    if GamePlay.Player.Inventory.FindResource(KeyItemResource) = -1 then
    begin
      Notifications.Show('You need a key to open this door');
      RejectGateExitBox;
    end else
    if GamePlay.Player.Inventory.FindResource(Sword) = -1 then
    begin
      Notifications.Show('Better find a wepon first to protect yourself in the castle');
      RejectGateExitBox;
    end else
    begin
      SoundEngine.Sound(stKeyDoorUse);
      LevelFinished('castle_hall');
    end;
  end else
  begin
    TeleportWork(Teleport1, Teleport1Box, Teleport1Destination);
    TeleportWork(Teleport2, Teleport2Box, Teleport2Destination);

    if (not SacrilegeAmbushDone) and
      FSacrilegeBox.Contains(Player.Translation) then
    begin
      SacrilegeAmbushDone := true;
      SacrilegeAmbush;
    end;

    if (not SwordAmbushDone) and
      (GamePlay.Player.Inventory.FindResource(Sword) <> -1) then
    begin
      SwordAmbushDone := true;
      SwordAmbush;
    end;
  end;

  if Time - CartLastSoundTime > CartSoundRepeatTime then
  begin
    CartLastSoundTime := Time;
    SoundEngine.Sound3d(stCreak, CartSoundPosition);
  end;
end;

{ TTowerElevatorButton ------------------------------------------------------- }

type
  TTowerElevatorButton = class(TCastleScene)
    MovingElevator: TCastleLinearMoving;
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

function TTowerElevatorButton.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  if Distance > 10 then
    NotificationInteractFailed(
      'You see a button. You''re too far to reach it from here') else
  begin
    { play from the beginning }
    PlayAnimation('animation', false);
    MovingElevator.GoOtherPosition;
  end;
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  TowerLevelPath: string;
begin
  inherited;

  TowerLevelPath := LevelsPath + 'tower/';

  Elevator := TCastleScene.Create(Self);
  Elevator.Load(TowerLevelPath + 'elevator.wrl');
  Elevator.ProcessEvents := true;
  Elevator.Spatial := [ssDynamicCollisions];

  { Do not build collisions structure, let it collide as a bounding box.
    Doing it properly (with PrepareForCollisions = true) would require
    reexporting as castle-anim-frames (with it's bbox recorded). }
  ElevatorButton := TTowerElevatorButton.Create(Self);
  ElevatorButton.Load(TowerLevelPath + 'elevator_button.kanim');
  ElevatorButton.ProcessEvents := true;

  MovingElevator := TCastleLinearMoving.Create(Self);
  MovingElevator.Add(Elevator);
  MovingElevator.Add(ElevatorButton);
  MovingElevator.MoveTime := 10.0;
  MovingElevator.TranslationEnd := Vector3(0, 0, 122);
  MovingElevator.SoundGoEndPosition := stElevator;
  MovingElevator.SoundGoEndPositionLooping := true;
  MovingElevator.SoundGoBeginPosition := stElevator;
  MovingElevator.SoundGoBeginPositionLooping := true;
  MovingElevator.SoundTracksCurrentPosition := true;
  Level.RootTransform.Add(MovingElevator);

  TTowerElevatorButton(ElevatorButton).MovingElevator := MovingElevator;
end;

{ TGateExit ------------------------------------------------------------------ }

type
  TGateExit = class(TCastleScene)
    Boss: TCreature;
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

function TGateExit.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  if Distance > 10 then
    NotificationInteractFailed(
      'You see a door. You''re too far to open it from here') else
  begin
    if Player.Inventory.FindResource(RedKeyItemResource) <> -1 then
    begin
      if (Boss <> nil) and (not Boss.Dead) then
      begin
        Player.Hurt(2 + Random(5), Vector3(0, -1, 0), 2, nil);
        SoundEngine.Sound(stEvilLaugh);
        Notifications.Show('No exit for the one who does not fight');
      end else
      begin
        LevelFinished('');
      end;
    end else
      NotificationInteractFailed('You need an appropriate key to open this door');
  end;
end;

{ TCagesLevel ---------------------------------------------------------------- }

constructor TCagesLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
begin
  inherited;

  SpidersAppearing := TCastleTransform.Create(Self);
  Level.RootTransform.Add(SpidersAppearing);
  NextSpidersAppearingTime := 0;

  HintOpenDoorScript := MainScene.Node('HintOpenDoorBoxScript');

  FEndSequence := TCastleScene.Create(Self);
  FEndSequence.Load(LevelsPath + 'end_sequence/end_sequence_final.wrl');
  FEndSequence.ProcessEvents := true;
  FEndSequence.Spatial := [ssDynamicCollisions];
  FEndSequence.Exists := false;
  { Even when FEndSequence will exist, we will not check for collisions
    with it --- no reason to waste time, no collisions will be possible
    as player's move along the EndSequence will be programmed. }
  FEndSequence.Collides := false;
  FEndSequence.CastShadowVolumes := false; { shadow is not visible anyway }
  Level.RootTransform.Add(FEndSequence);

  FGateExit := TGateExit.Create(Self);
  FGateExit.Load(LevelsPath + 'cages/cages_gate_exit.wrl');
  FGateExit.ProcessEvents := true;
  FGateExit.CastShadowVolumes := false; { shadow is not visible anyway }
  Level.RootTransform.Add(FGateExit);
end;

procedure TCagesLevel.PlaceholdersEnd;

  function FindCreatureResource(Resource: TCreatureResource): TCreature;
  var
    I: Integer;
  begin
    for I := 0 to Level.RootTransform.Count - 1 do
      if Level.RootTransform[I] is TCreature then
      begin
        Result := TCreature(Level.RootTransform[I]);
        if Result.Resource = Resource then
          Exit;
      end;
    Result := nil;
  end;

begin
  inherited;
  Boss := FindCreatureResource(SpiderQueen);
  TGateExit(FGateExit).Boss := Boss;
end;

procedure TCagesLevel.SetGameWinAnimation(Value: TGameWinAnimation);
var
  OldGameWinBackground, NewGameWinBackground: boolean;
  GameWinBackground: TAbstractBackgroundNode;
begin
  OldGameWinBackground := FEndSequence.Exists;

  FGameWinAnimation := Value;
  FEndSequence.Exists := GameWinAnimation >= gwaAnimateTo2;
  FGateExit.Exists    := GameWinAnimation <  gwaAnimateTo2;

  { make the background for game win animation visible }
  NewGameWinBackground := FEndSequence.Exists;
  if (not OldGameWinBackground) and NewGameWinBackground then
  begin
    GameWinBackground := SceneManager.Items.MainScene.RootNode.FindNodeByName(
      TAbstractBackgroundNode, 'GameWinBackground', false) as TAbstractBackgroundNode;
    GameWinBackground.EventSet_bind.Send(true);
  end;
end;

function TCagesLevel.BossIndicator(out Life, MaxLife: Single): boolean;
begin
  Result := (Boss <> nil) and (not Boss.Dead);
  if Result then
  begin
    Life := Boss.Life;
    MaxLife := Boss.MaxLife;
  end;
end;

procedure TCagesLevel.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  { Some SpiderLargeRadius is used to not put spider inside the wall. }
  SpiderLargeRadius = 2;
  MinSpiderX = -11.0  + SpiderLargeRadius;
  MaxSpiderX = 69.0   - SpiderLargeRadius;
  MinSpiderY = -123.0 + SpiderLargeRadius;
  MaxSpiderY = 162.0  - SpiderLargeRadius;

  { Remember to make it -1 lower than actual ceiling geometry,
    otherwise the spiders will be created on the ceiling of the model... }
  SpiderZ = 69.0;

  procedure AppearSpider(const Position: TVector3);
  var
    SpiderScene: TCastleScene;
    SpiderAppearing: TCastleScene;
  begin
    SpiderScene := TCastleScene.Create(Self);
    SpiderScene.Url := 'castle-data:/creatures/spider/spider_stand.wrl';

    SpiderAppearing := TCastleScene.Create(Self);
    SpiderAppearing.Load('castle-data:/creatures/spider/appearing.x3dv');
    SpiderAppearing.Add(SpiderScene);
    SpiderAppearing.Collides := false;
    SpiderAppearing.Translation := Position;
    SpidersAppearing.Add(SpiderAppearing);
  end;

  function RandomSpiderXY: TVector3;
  begin
    Result[0] := MapRange(Random, 0.0, 1.0, MinSpiderX, MaxSpiderX);
    Result[1] := MapRange(Random, 0.0, 1.0, MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

  function RandomSpiderXYAroundPlayer: TVector3;
  const
    RandomDist = 10.0;
  begin
    Result[0] := Player.Translation[0] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[0] := Clamped(Result[0], MinSpiderX, MaxSpiderX);
    Result[1] := Player.Translation[1] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[1] := Clamped(Result[1], MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

  function CreaturesCount: Cardinal;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Level.RootTransform.Count - 1 do
      if Level.RootTransform[I] is TCreature then
        Inc(Result);
  end;

const
  SpidersFallingSpeed = 25;
  CreaturesCountToAddSpiders = 20;
var
  AboveHeight: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3;
  SpiderMoveDistance, SpiderRadius: Single;
  SpiderAppearing: TCastleTransform;
  TorchLight: PLightInstance;
begin
  inherited;

  if (Player = nil) then Exit;

  if not GameWin then
  begin
    { Torch light modify, to make an illusion of unstable light }
    TorchLight := SceneManager.Items.MainScene.GlobalLights.FindName('MainHallTorchLight');
    Check(TorchLight <> nil, 'Torch light not found on cages level');
    TorchLight^.Node.Intensity := Clamped(
        TorchLight^.Node.Intensity +
          MapRange(Random, 0, 1, -5.0, 5.0) * SecondsPassed,
        0.5, 1);

    { Maybe appear new spiders }
    if { Spider.Prepared may be false here only if
         --debug-no-creatures was specified. In this case,
         leave Spider unprepared and don't use spider's on this level. }
       Spider.Prepared then
    begin
      if NextSpidersAppearingTime = 0 then
      begin
        if Time > 1 then
        begin
          NextSpidersAppearingTime := Time + 5 + Random(20);
          for I := 1 to 5 + Random(3) do
            AppearSpider(RandomSpiderXY);
        end;
      end else
      if Time >= NextSpidersAppearingTime then
      begin
        NextSpidersAppearingTime := Time + 2 + Random(10);
        if CreaturesCount < CreaturesCountToAddSpiders then
          for I := 1 to 1 + Random(3) do
            AppearSpider(RandomSpiderXYAroundPlayer);
      end;
    end;

    { Move spiders down }
    I := 0;
    SpiderRadius := Spider.Radius(Level.RootTransform.GravityUp);
    AboveHeight := MaxSingle;
    while I < SpidersAppearing.Count do
    begin
      SpiderAppearing := SpidersAppearing[I];
      SpiderPosition := SpiderAppearing.Translation;
      SpiderAppearing.Height(SpiderPosition, AboveHeight);
      if AboveHeight < SpiderRadius * 2 then
      begin
        SpiderDirection := Player.Translation - SpiderPosition;
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, Level.RootTransform.GravityUp);
        SpiderCreature := Spider.CreateCreature(SceneManager.LevelProperties, SpiderPosition, SpiderDirection);
        SpiderCreature.Sound3d(stSpiderAppears, 1.0);
        FreeAndNil(SpiderAppearing); { it will be automatically removed from SpidersAppearing list }
      end else
      begin
        { calculate SpiderMoveDistance }
        SpiderMoveDistance := SpidersFallingSpeed * SecondsPassed;
        MinVar(SpiderMoveDistance, AboveHeight - SpiderRadius);
        SpiderPosition.Data[2] -= SpiderMoveDistance;
        SpiderAppearing.Translation := SpiderPosition;
        Inc(I);
      end;
    end;
  end else
    { No longer any need to show this hint. }
    (HintOpenDoorScript.Field('done') as TSFBool).Send(true);
end;

procedure TCagesLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword and 1 bow, to have weapons. }
  NewPlayer.PickItem(Sword.CreateItem(1));
  NewPlayer.PickItem(Bow.CreateItem(1));
end;

{ TDoomLevelDoor ------------------------------------------------------------- }

constructor TDoomLevelDoor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pushes := false;
  SoundGoEndPosition := stDoorOpen;
  SoundGoBeginPosition := stDoorClose;
  CastShadowVolumes := false; { looks bad }
end;

procedure TDoomLevelDoor.BeforeTimeIncrease(const NewTime: TFloatTime);

  function SomethingWillBlockClosingDoor: boolean;
  var
    DoorBox: TBox3D;
    I: Integer;
  begin
    DoorBox := (inherited BoundingBox).Translate(
      GetTranslationFromTime(NewTime) - Translation);

    Result := false;

    for I := 0 to World.Count - 1 do
      if World[I].CollidesWithMoving then
      begin
        Result := DoorBox.Collision(World[I].BoundingBox);
        if Result then
          Exit;
      end;
  end;

begin
  inherited;

  { Check the closing doors: if some 3D item with CollidesWithMoving=@true
    will collide after Time change to NewTime, then we must open door again. }

  if (not EndPosition) and
    (Time - EndPositionStateChangeTime < MoveTime) and
    SomethingWillBlockClosingDoor then
    RevertGoEndPosition;
end;

procedure TDoomLevelDoor.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  if EndPosition and
    (Time - EndPositionStateChangeTime >
      MoveTime + StayOpenTime) then
    GoBeginPosition;
end;

function TDoomLevelDoor.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  if Distance > 7 then
    NotificationInteractFailed('You see a door. You''re too far to open it from here') else
  { Only if the door is completely closed
    (and not during closing right now) we allow player to open it. }
  if not CompletelyBeginPosition then
    NotificationInteractFailed('You see a door. It''s already open') else
    GoEndPosition;
end;

{ TElevator9a9b ------------------------------------------------------------------ }

type
  TElevator9a9b = class(TCastleScene)
  public
    MovingElevator9a9b: TCastleLinearMoving;
    Elevator9a9bPickBox: TBox3D;
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
  end;

function TElevator9a9b.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  Result := MovingElevator9a9b.CompletelyBeginPosition and
    Elevator9a9bPickBox.Contains(Player.Translation);

  if Result then
  begin
    if Distance > 10 then
      NotificationInteractFailed(
        'You''re too far to reach it from here') else
      MovingElevator9a9b.GoEndPosition;
  end;
end;

{ TExitButton ------------------------------------------------------------------ }

type
  TExitButton = class(TCastleScene)
  public
    ExitMessagePending: boolean;
    function PointingDevicePress(const Pick: TRayCollisionNode;
      const Distance: Single): Boolean; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

function TExitButton.PointingDevicePress(const Pick: TRayCollisionNode;
  const Distance: Single): Boolean;
begin
  Result := inherited;
  if Result then Exit;
  if SceneManager.PlayerBlocked then Exit;

  if Distance > 5 then
    NotificationInteractFailed(
      'You''re too far to reach it from here') else
    begin
      SoundEngine.Sound(stDoomExitButton);
      Player.Life := 0;
      ExitMessagePending := true;
    end;
end;

procedure TExitButton.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  if ExitMessagePending and (not Player.WalkNavigation.FallingOnTheGround) then
  begin
    { ExitMessagePending is displayed when player FallOnTheGround effect
      (when dying) ended. }
    MessageOK(Window,
      'Congratulations ! You finished the game. ' +
      'Now you can just die and go to hell.' +nl+
      nl+
      'Seriously: I was just too lazy to implement any kind of real ' +
      '"game finished" sequence for the "Doom" level. So I figured ' +
      'out that I may as well kill the player now, just in case ' +
      'you didn''t see the death animation yet ? :)' +nl+
      nl+
      'Now really seriously: I hope you enjoyed the game. ' +
      'This is only the beginning of a development of a real game ' +
      '--- you know, with real storyline, and just everything much ' +
      'much better. ' +
      'So check out for updates on our WWW page ' +
      '[http://castle-engine.sourceforge.net/castle.php]. ' +
      'Oh, and this is open-source game, so if you can, ' +
      'you''re most welcome to contribute!');
    ExitMessagePending := false;
  end;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  DoomDoorsPathPrefix: string;

  function MakeDoor(const URL: string): TDoomLevelDoor;
  var
    Scene: TCastleScene;
  begin
    Scene := TCastleScene.Create(Self);
    Scene.Load(DoomDoorsPathPrefix + URL);
    Scene.ProcessEvents := true;
    Scene.Spatial := [ssDynamicCollisions];

    Result := TDoomLevelDoor.Create(Self);
    Result.Add(Scene);

    { Although I didn't know it initially, it turns out that all doors
      on Doom E1M1 level (maybe all doors totally ?) have the same
      values for parameters below. }
    Result.MoveTime := 1.0;
    Result.TranslationEnd := Vector3(0, 0, 3.5);
    Result.StayOpenTime := 5.0;
  end;

begin
  inherited;

  DoomDoorsPathPrefix := LevelsPath + 'doom/e1m1/';

  Level.RootTransform.Add(MakeDoor('door2_3_closed.wrl'));
  Level.RootTransform.Add(MakeDoor('door4_5_closed.wrl'));
  Level.RootTransform.Add(MakeDoor('door4_7_closed.wrl'));
  Level.RootTransform.Add(MakeDoor('door5_6_closed.wrl'));

  FakeWall := TCastleScene.Create(Self);
  FakeWall.Load(DoomDoorsPathPrefix + 'fake_wall_final.wrl');
  FakeWall.ProcessEvents := true;
  FakeWall.Collides := false;
  FakeWall.CastShadowVolumes := false;
  Level.RootTransform.Add(FakeWall);

  Elevator49 := TCastleScene.Create(Self);
  Elevator49.Load(DoomDoorsPathPrefix + 'elevator4_9_final.wrl');
  Elevator49.ProcessEvents := true;

  MovingElevator49 := TCastleLinearMoving.Create(Self);
  MovingElevator49.Add(Elevator49);
  MovingElevator49.MoveTime := 3.0;
  MovingElevator49.TranslationEnd := Vector3(0, 0, -6.7);
  MovingElevator49.SoundGoEndPosition := stElevator;
  MovingElevator49.SoundGoEndPositionLooping := true;
  MovingElevator49.SoundGoBeginPosition := stElevator;
  MovingElevator49.SoundGoBeginPositionLooping := true;
  MovingElevator49.SoundTracksCurrentPosition := true;
  MovingElevator49.CastShadowVolumes := false;
  Level.RootTransform.Add(MovingElevator49);

  Elevator9a9b := TElevator9a9b.Create(Self);
  Elevator9a9b.Load(DoomDoorsPathPrefix + 'elevator_9a_9b_final.wrl');
  Elevator9a9b.ProcessEvents := true;

  MovingElevator9a9b := TCastleLinearMoving.Create(Self);
  MovingElevator9a9b.Add(Elevator9a9b);
  MovingElevator9a9b.MoveTime := 3.0;
  MovingElevator9a9b.TranslationEnd := Vector3(0, 0, -7.5);
  MovingElevator9a9b.SoundGoEndPosition := stElevator;
  MovingElevator9a9b.SoundGoEndPositionLooping := true;
  MovingElevator9a9b.SoundGoBeginPosition := stElevator;
  MovingElevator9a9b.SoundGoBeginPositionLooping := true;
  MovingElevator9a9b.SoundTracksCurrentPosition := true;
  MovingElevator9a9b.CastShadowVolumes := false;
  Level.RootTransform.Add(MovingElevator9a9b);

  ExitButton := TExitButton.Create(Self);
  ExitButton.Load(DoomDoorsPathPrefix + 'exit_button_final.wrl');
  ExitButton.ProcessEvents := true;
  ExitButton.CastShadowVolumes := false;
  Level.RootTransform.Add(ExitButton);

  TElevator9a9b(Elevator9a9b).MovingElevator9a9b := MovingElevator9a9b;
end;

function TDoomE1M1Level.Placeholder(const Shape: TShape;
  const PlaceholderName: string): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if PlaceholderName = 'Elevator49DownBox' then
  begin
    Elevator49DownBox := Shape.BoundingBox;
    Exit(true);
  end;

  if PlaceholderName = 'Elev9a9bPickBox' then
  begin
    TElevator9a9b(Elevator9a9b).Elevator9a9bPickBox := Shape.BoundingBox;
    Exit(true);
  end;
end;

procedure TDoomE1M1Level.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  NewPlayer.PickItem(Bow.CreateItem(1));
  NewPlayer.PickItem(Quiver.CreateItem(10));
end;

procedure TDoomE1M1Level.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  if Player = nil then Exit;

  if MovingElevator49.CompletelyBeginPosition and
     Elevator49DownBox.Contains(Player.Translation) then
  begin
    MovingElevator49.GoEndPosition;
  end;

  if MovingElevator9a9b.CompletelyEndPosition and
     (Time - MovingElevator9a9b.EndPositionStateChangeTime >
       MovingElevator9a9b.MoveTime +
       { This is the time for staying in lowered position. }
       2.0) then
    MovingElevator9a9b.GoBeginPosition;
end;

{ TGateBackgroundLevel ------------------------------------------------------- }

constructor TGateBackgroundLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  Water: TCastleScene;
begin
  inherited;

  Water := TCastleScene.Create(Self);
  Water.Load(LevelsPath + 'gate_background/water.kanim');
  Water.ProcessEvents := true;
  Water.CastShadowVolumes := false; { water shadow would look awkward }
  { No octrees created for water (because in normal usage, player will not
    walk on this level). For safety, Collides set to @false, in case
    user enters this level by debug menu. }
  Water.Collides := false;
  Water.PlayAnimation('animation', true);
  Level.RootTransform.Add(Water);
end;

{ TFountainLevel ------------------------------------------------------------- }

constructor TFountainLevel.Create(const AOwner: TComponent;
  const ALevelProperties: TLevelProperties;
  const MainScene: TCastleScene; const DOMElement: TDOMElement);
var
  Fountain: TSceneWaterShader;
  LoadWaterAnimation: boolean;
begin
  inherited;

  LevelFountainProcess(MainScene.RootNode);

  if DOMElement.AttributeBoolean('load_water_animation', LoadWaterAnimation)
    and LoadWaterAnimation then
  begin
    { load Fountain animation }
    Fountain := TSceneWaterShader.Create(Self);
    Fountain.Load(LevelsPath + 'fountain/water_stream/fountain.kanim');
    Fountain.PrepareResources([prRenderSelf, prBoundingBox], false, SceneManager.PrepareParams);
    Fountain.FreeResources([frTextureDataInNodes]);
    Fountain.CastShadowVolumes := false; { not manifold }
    Fountain.Collides := false;
    Fountain.RenderOptions.BlendingDestinationFactor := bdOneMinusSrcAlpha;
    Fountain.TimePlayingSpeed := 1.5;
    Fountain.TimePlaying := true;

    Level.RootTransform.Add(Fountain);
  end;
end;

procedure TFountainLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword. Otherwise player would start the level
    without any weapon, and there's no weapon to be found on
    the level... }
  NewPlayer.PickItem(Sword.CreateItem(1));
end;

initialization
  { register our level logic classes }
  LevelLogicClasses['Cages'] := TCagesLevel;
  LevelLogicClasses['Gate'] := TGateLevel;
  LevelLogicClasses['GateBackground'] := TGateBackgroundLevel;
  LevelLogicClasses['CastleHall'] := TCastleHallLevel;
  LevelLogicClasses['DoomE1M1'] := TDoomE1M1Level;
  LevelLogicClasses['Tower'] := TTowerLevel;
  LevelLogicClasses['Fountain'] := TFountainLevel;
end.
