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

{ TLevel specialized descendants. }
unit CastleLevelSpecific;

interface

uses VRMLGLAnimation, VRMLFlatSceneGL, Boxes3d, VectorMath,
  CastlePlayer, CastleLevel, VRMLTriangleOctree, BackgroundGL,
  ALSourceAllocator, CastleSound, Matrix, VRMLNodes, DOM,
  CastleCreatures, ShadowVolumesHelper;

const
  CastleHallWerewolvesCount = 4;

type
  TCastleHallLevel = class(TLevel)
  private
    Symbol: TLevelSimpleAnimatedObject;
    Button: TLevelSimpleAnimatedObject;

    StairsBlocker: TLevelStaticObject;

    FLevelExitBox: TBox3d;

    WerewolfAppearPosition: array [0..CastleHallWerewolvesCount - 1] of TVector3Single;
    WerewolfAppeared: boolean;
    WerewolfCreature: array [0..CastleHallWerewolvesCount - 1] of TWerewolfCreature;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;

    procedure Idle(const CompSpeed: Single); override;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; override;
  end;

  TGateLevel = class(TLevel)
  private
    FGateExitBox: TBox3d;

    Teleport: TVRMLFlatSceneGL;
    FTeleport1Box, FTeleport2Box: TBox3d;

    Teleport1Rotate: Single;
    Teleport2Rotate: Single;

    Teleport1Destination: TVector3Single;
    Teleport2Destination: TVector3Single;

    SacrilegeAmbushStartingPosition: array [0..5] of TVector3Single;
    SwordAmbushStartingPosition: array [0..2] of TVector3Single;

    SacrilegeAmbushDone: boolean;
    SwordAmbushDone: boolean;

    FSacrilegeBox: TBox3d;

    CartLastSoundTime: Single;
    CartSoundPosition: TVector3Single;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;
    destructor Destroy; override;

    function CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single); override;

    procedure Render(const Frustum: TFrustum); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper); override;
  end;

  TTowerLevel = class(TLevel)
  private
    MovingElevator: TLevelLinearMovingObject;
    Elevator: TLevelStaticObject;
    ElevatorButton: TLevelSimpleAnimatedObject;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;
  end;

  TCagesLevel = class(TLevel)
  private
    FSpidersAppearing: TDynVector3SingleArray;
    NextSpidersAppearingTime: Single;

    HintOpenDoor: TLevelHintArea;

    FGateExit: TLevelStaticObject;

    FDoEndSequence: boolean;

    FEndSequence: TLevelStaticObject;
    procedure SetDoEndSequence(Value: boolean);
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;
    destructor Destroy; override;

    procedure Idle(const CompSpeed: Single); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Render(const Frustum: TFrustum); override;

    procedure RenderShadowVolume(
      ShadowVolumesHelper: TShadowVolumesHelper); override;

    { True means that GateExit will not be rendered (or collided)
      and EndSequence will be rendered. }
    property DoEndSequence: boolean
      read FDoEndSequence write SetDoEndSequence default false;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;

    function Background: TBackgroundGL; override;
  end;

  TDoomLevelDoor = class(TLevelLinearMovingObject)
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string);

    StayOpenTime: Single;

    procedure BeforeIdle(const NewAnimationTime: TAnimationTime); override;
    procedure Idle; override;

    property MovePushesOthers default false;

    { No way to express this:
    property SoundGoBeginPosition default stDoorClose;
    property SoundGoEndPosition default stDoorOpen;
    }
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TVRMLNode);

    FakeWall: TLevelStaticObject;

    MovingElevator49: TLevelLinearMovingObject;
    Elevator49: TLevelStaticObject;
    Elevator49DownBox: TBox3d;

    MovingElevator9a9b: TLevelLinearMovingObject;
    Elevator9a9b: TLevelStaticObject;
    Elevator9a9bPickBox: TBox3d;

    ExitButton: TLevelStaticObject;
    ExitMessagePending: boolean;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;
    destructor Destroy; override;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Idle(const CompSpeed: Single); override;
  end;

  TGateDemoLevel = class(TLevel)
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ADemo: boolean); override;
  end;

function CastleLevelsPath: string;

implementation

uses KambiFilesUtils, SysUtils, Object3dAsVRML, KambiUtils,
  OpenGLh, KambiGLUtils, KambiStringUtils, GLWinMessages,
  CastlePlay, CastleTimeMessages, CastleInputs,
  CastleItems, CastleThunder, CastleLevelAvailable, CastleWindow;

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  CastleHallLevelPath: string;
begin
  inherited;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := TLevelSimpleAnimatedObject.Create(Self,
    LoadLevelAnimation(CastleHallLevelPath + 'symbol.kanim', true, false));
  Symbol.CastsShadow := false; { shadow would not be visible anyway }
  Objects.Add(Symbol);

  Button := TLevelSimpleAnimatedObject.Create(Self,
    LoadLevelAnimation(CastleHallLevelPath + 'button.kanim', true, false));
  Button.CastsShadow := false; { strange ghost shadow on symbol would be visible }
  Objects.Add(Button);

  StairsBlocker := TLevelStaticObject.Create(Self,
    CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl', false);
  StairsBlocker.CastsShadow := false; { shadow would not be visible anyway }
  Objects.Add(StairsBlocker);
end;

procedure TCastleHallLevel.ChangeLevelScene;

  function BoxDownPosition(const Box: TBox3d): TVector3Single;
  begin
    Result[0] := (Box[0, 0] + Box[1, 0]) / 2;
    Result[1] := (Box[0, 1] + Box[1, 1]) / 2;
    Result[2] := Box[0, 2];
  end;

var
  TempBox: TBox3d;
  I: Integer;
begin
  inherited;
  RemoveBoxNodeCheck(FLevelExitBox, 'LevelExitBox');

  for I := 0 to CastleHallWerewolvesCount - 1 do
  begin
    RemoveBoxNodeCheck(TempBox, 'WerewolfAppear_' + IntToStr(I));
    WerewolfAppearPosition[I] := BoxDownPosition(TempBox);
  end;
end;

procedure TCastleHallLevel.Idle(const CompSpeed: Single);
const
  WerewolfFirstLight = 1;

  procedure WerewolfAppear;
  var
    I: Integer;
    LightNode: TNodeGeneralPositionalLight;
  begin
    Assert(not WerewolfAppeared);

    for I := 0 to CastleHallWerewolvesCount - 1 do
    begin
      WerewolfCreature[I] := Werewolf.CreateDefaultCreature(
        WerewolfAppearPosition[I],
        VectorSubtract(Player.Navigator.CameraPos, WerewolfAppearPosition[I]),
        AnimationTime, Werewolf.DefaultMaxLife) as TWerewolfCreature;
      Creatures.Add(WerewolfCreature[I]);
    end;

    WerewolfAppeared := true;

    WerewolfCreature[0].Howl(true);

    { change the lights }
    Headlight.AmbientIntensity := 0.8;
    Headlight.Color := Vector3Single(1, 0, 0);
    Headlight.Intensity := 0.2;
    Level.Headlight.Render(0, false { it should be already enabled });

    for I := 0 to CastleHallWerewolvesCount - 1 do
    begin
      LightNode := LightSet.Lights.Items[I + WerewolfFirstLight].LightNode as
        TNodeGeneralPositionalLight;
      LightNode.FdColor.Value := Vector3Single(1, 0, 0);
      LightNode.FdAttenuation.Value := Vector3Single(1, 0.1, 0);
      LightNode.FdKambiShadows.Value := true;
    end;

    LightSet.Lights.Items[0].LightNode.FdKambiShadows.Value := true;
    LightSet.Lights.Items[0].LightNode.FdKambiShadowsMain.Value := true;

    LightSet.CalculateLights;
  end;

var
  WerewolfAliveCount: Cardinal;

  procedure DestroyStairsBlocker;
  begin
    if StairsBlocker.Exists then
    begin
      StairsBlocker.Exists := false;
      SoundEngine.Sound3d(stStairsBlockerDestroyed, Box3dMiddle(StairsBlocker.Scene.BoundingBox));
    end;
  end;

  procedure WerewolfShowLights;
  var
    I: Integer;
    LightNode: TNodeGeneralPositionalLight;
  begin
    if WerewolfAliveCount = 0 then
    begin
      { turn light over stairs to next level }
      LightNode := LightSet.Lights.Items[WerewolfFirstLight].LightNode as
        TNodeGeneralPositionalLight;
      LightNode.FdLocation.Value := Box3dMiddle(StairsBlocker.Scene.BoundingBox);
      LightNode.FdOn.Value := true;

      for I := 1 to CastleHallWerewolvesCount - 1 do
      begin
        LightNode := LightSet.Lights.Items[I + WerewolfFirstLight].LightNode as
          TNodeGeneralPositionalLight;
        LightNode.FdOn.Value := false;
      end;
    end else
    begin
      { turn light for each alive werewolf }
      for I := 0 to CastleHallWerewolvesCount - 1 do
      begin
        LightNode := LightSet.Lights.Items[I + WerewolfFirstLight].LightNode as
          TNodeGeneralPositionalLight;
        LightNode.FdOn.Value := not WerewolfCreature[I].Dead;
        LightNode.FdLocation.Value := WerewolfCreature[I].MiddlePosition;
      end;
    end;
    LightSet.CalculateLights;
  end;

  function GetWerewolfAliveCount: Cardinal;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to CastleHallWerewolvesCount - 1 do
      if not WerewolfCreature[I].Dead then
        Inc(Result);
  end;

begin
  inherited;

  if Box3dPointInside(Player.Navigator.CameraPos, FLevelExitBox) then
  begin
    LevelFinished('cages');
  end;

  if Button.Started and
    (AnimationTime - Button.PlayStartTime > Button.Animation.TimeDuration) then
  begin
    if not Symbol.Started then
    begin
      Symbol.Play;
      Symbol.Collides := false;
      SoundEngine.Sound3d(stCastleHallSymbolMoving, Vector3Single(0, 0, 0));

      WerewolfAppear;
    end;
  end;

  if WerewolfAppeared then
  begin
    WerewolfAliveCount := GetWerewolfAliveCount;
    WerewolfShowLights;
    if WerewolfAliveCount = 0 then
      DestroyStairsBlocker;
  end;
end;

procedure TCastleHallLevel.Picked(const Distance: Single;
  CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
begin
  inherited;

  if CollisionInfo.Hierarchy.IsLast(StairsBlocker) then
  begin
    InteractionOccured := true;
    TimeMessageInteractFailed('You are not able to open it');
  end else
  if CollisionInfo.Hierarchy.IsLast(Button) then
  begin
    InteractionOccured := true;
    if Distance < 10.0 then
    begin
      if Button.Started then
        TimeMessageInteractFailed('Button is already pressed') else
      begin
        Button.Play;
        TimeMessage('You press the button');
      end;
    end else
      TimeMessageInteractFailed('You see a button. You cannot reach it from here');
  end;
end;

procedure TCastleHallLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword. Otherwise player would start the level
    without any weapon, and there's no weapon to be found on
    the level... }
  NewPlayer.PickItem(TItem.Create(Sword, 1));
end;

function TCastleHallLevel.BossCreatureIndicator(
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
    for I := 0 to CastleHallWerewolvesCount - 1 do
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

constructor TGateLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  Cart: TLevelSimpleAnimatedObject;
  GateLevelPath: string;
begin
  inherited;

  GateLevelPath := CastleLevelsPath + 'gate' + PathDelim;

  Teleport := LoadLevelScene(GateLevelPath + 'teleport.wrl', false, false);

  Cart := TLevelSimpleAnimatedObject.Create(Self, LoadLevelAnimation(
    GateLevelPath + 'cart.kanim', true, true));
  Cart.CollisionUseLastScene := true;
  Objects.Add(Cart);
  Cart.Play;

  CartSoundPosition := Box3dMiddle(Cart.Animation.FirstScene.BoundingBox);

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
end;

destructor TGateLevel.Destroy;
begin
  FreeAndNil(Teleport);
  inherited;
end;

procedure TGateLevel.ChangeLevelScene;

  function AmbushStartingPos(const Box: TBox3d): TVector3Single;
  begin
    Result[0] := (Box[0, 0] + Box[1, 0]) / 2;
    Result[1] := (Box[0, 1] + Box[1, 1]) / 2;
    Result[2] := Box[0, 2];
  end;

var
  TempBox: TBox3d;
  I: Integer;
begin
  inherited;

  RemoveBoxNodeCheck(FGateExitBox, 'GateExitBox');

  RemoveBoxNodeCheck(FTeleport1Box, 'Teleport1Box');
  RemoveBoxNodeCheck(FTeleport2Box, 'Teleport2Box');

  RemoveBoxNodeCheck(FSacrilegeBox, 'SacrilegeBox');

  Teleport1Destination := Box3dMiddle(FTeleport2Box);
  Teleport1Destination[0] += 2;
  Teleport1Destination[1] += 2;

  Teleport2Destination := Box3dMiddle(FTeleport1Box);
  Teleport2Destination[0] -= 2;
  Teleport2Destination[1] -= 2;

  for I := 0 to High(SacrilegeAmbushStartingPosition) do
  begin
    RemoveBoxNodeCheck(TempBox, 'SacrilegeGhost_' + IntToStr(I));
    SacrilegeAmbushStartingPosition[I] := AmbushStartingPos(TempBox);
  end;

  for I := 0 to High(SwordAmbushStartingPosition) do
  begin
    RemoveBoxNodeCheck(TempBox, 'SwordGhost_' + IntToStr(I));
    SwordAmbushStartingPosition[I] := AmbushStartingPos(TempBox);
  end;
end;

procedure TGateLevel.Idle(const CompSpeed: Single);

  procedure RejectGateExitBox;
  var
    NewPosition: TVector3Single;
  begin
    NewPosition := Player.Navigator.CameraPos;
    { Although I do him knockback, I also change the position
      to make sure that he is thrown outside of FGateExitBox. }
    NewPosition[1] := FGateExitBox[0, 1] - 0.1;
    Player.Navigator.CameraPos := NewPosition;

    Player.Knockback(0, 2, Vector3Single(0, -1, 0));
  end;

  procedure TeleportWork(const TeleportBox: TBox3d;
    const Destination: TVector3Single);
  begin
    if Box3dPointInside(Player.Navigator.CameraPos, TeleportBox) then
    begin
      Player.Navigator.CameraPos := Destination;
      Player.Navigator.CancelFallingDown;

      SoundEngine.Sound(stTeleport);
    end;
  end;

  procedure SacrilegeAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
    Creature: TCreature;
  begin
    SoundEngine.Sound(stSacrilegeAmbush);
    for I := 0 to High(SacrilegeAmbushStartingPosition) do
    begin
      CreaturePosition := SacrilegeAmbushStartingPosition[I];
      CreatureDirection := VectorSubtract(Player.Navigator.CameraPos,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, Ghost.DefaultMaxLife);
      Creatures.Add(Creature);
    end;
  end;

  procedure SwordAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
    Creature: TCreature;
  begin
    for I := 0 to High(SwordAmbushStartingPosition) do
    begin
      CreaturePosition := SwordAmbushStartingPosition[I];
      CreatureDirection := VectorSubtract(Player.Navigator.CameraPos,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, Ghost.DefaultMaxLife);
      Creatures.Add(Creature);
    end;
  end;

const
  { In seconds. }
  CartSoundRepeatTime = 10.0;
begin
  inherited;

  if Box3dPointInside(Player.Navigator.CameraPos, FGateExitBox) then
  begin
    if Player.Items.FindKind(KeyItemKind) = -1 then
    begin
      TimeMessage('You need a key to open this door');
      RejectGateExitBox;
    end else
    if Player.Items.FindKind(Sword) = -1 then
    begin
      TimeMessage('Better find a wepon first to protect yourself in the castle');
      RejectGateExitBox;
    end else
    begin
      SoundEngine.Sound(stKeyDoorUse);
      LevelFinished('castle_hall');
    end;
  end else
  begin
    Teleport1Rotate += 0.2 * CompSpeed;
    Teleport2Rotate += 0.2 * CompSpeed;
    TeleportWork(FTeleport1Box, Teleport1Destination);
    TeleportWork(FTeleport2Box, Teleport2Destination);

    if (not SacrilegeAmbushDone) and
      Box3dPointInside(Player.Navigator.CameraPos, FSacrilegeBox) then
    begin
      SacrilegeAmbushDone := true;
      SacrilegeAmbush;
    end;

    if (not SwordAmbushDone) and
      (Player.Items.FindKind(Sword) <> -1) and
      { not Ghost.PrepareRenderDone may happen only when run with
        --debug-no-creatures }
      Ghost.PrepareRenderDone then
    begin
      SwordAmbushDone := true;
      SwordAmbush;
    end;
  end;

  if AnimationTime - CartLastSoundTime > CartSoundRepeatTime then
  begin
    CartLastSoundTime := AnimationTime;
    SoundEngine.Sound3d(stCreak, CartSoundPosition);
  end;
end;

function TGateLevel.CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
  OctreeItemIndex: Integer): boolean;
var
  ItemPtr: POctreeItem;
begin
  Result := inherited;

  ItemPtr := @(Octree.OctreeItems.Items[OctreeItemIndex]);
  Result := Result or
    (ItemPtr^.State.LastNodes.Material.NodeName = 'MatWater');
end;

procedure TGateLevel.Render(const Frustum: TFrustum);

  procedure RenderTeleport(
    const TeleportRotation: Single;
    const TeleportBox: TBox3d;
    TransparentGroup: TTransparentGroup);
  begin
    if FrustumBox3dCollisionPossibleSimple(Frustum, TeleportBox) then
    begin
      glPushMatrix;
        glTranslatev(Box3dMiddle(TeleportBox));
        glRotatef(TeleportRotation, 1, 1, 0);
        Teleport.Render(nil, TransparentGroup);
      glPopMatrix;
    end;
  end;

begin
  RenderTeleport(Teleport1Rotate, FTeleport1Box, tgOpaque);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, tgOpaque);
  inherited;
  RenderTeleport(Teleport1Rotate, FTeleport1Box, tgTransparent);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, tgTransparent);
end;

procedure TGateLevel.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper);
begin
  { TODO: render teleport shadow quads }
  inherited;
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  ElevatorButtonSum: TLevelObjectSum;
  TowerLevelPath: string;
begin
  inherited;

  TowerLevelPath := CastleLevelsPath + 'tower' + PathDelim;

  Elevator := TLevelStaticObject.Create(
    Self, TowerLevelPath + 'elevator.wrl', false);

  ElevatorButton := TLevelSimpleAnimatedObject.Create(Self, LoadLevelAnimation(
    TowerLevelPath + 'elevator_button.kanim', true, false));

  ElevatorButtonSum := TLevelObjectSum.Create(Self);
  ElevatorButtonSum.List.Add(Elevator);
  ElevatorButtonSum.List.Add(ElevatorButton);

  MovingElevator := TLevelLinearMovingObject.Create(Self);
  MovingElevator.MovingObject := ElevatorButtonSum;
  MovingElevator.MoveTime := 10.0;
  MovingElevator.TranslationEnd.Init(0, 0, 122);
  MovingElevator.SoundGoEndPosition := stElevator;
  MovingElevator.SoundGoEndPositionLooping := true;
  MovingElevator.SoundGoBeginPosition := stElevator;
  MovingElevator.SoundGoBeginPositionLooping := true;
  MovingElevator.SoundTracksCurrentPosition := true;
  { no shadow, because looks bad: tower level has uninteresting light
    and elevator triggers artifacts because of BorderEdges. }
  MovingElevator.CastsShadow := false;
  Objects.Add(MovingElevator);
end;

procedure TTowerLevel.Picked(const Distance: Single;
  CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
begin
  inherited;

  if CollisionInfo.Hierarchy.IsLast(ElevatorButton) then
  begin
    InteractionOccured := true;
    if Distance > 10 then
      TimeMessageInteractFailed(
        'You see a button. You''re too far to reach it from here') else
    begin
      ElevatorButton.Play;
      MovingElevator.GoOtherPosition;
    end;
  end;
end;

{ TCagesLevel ---------------------------------------------------------------- }

constructor TCagesLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  BossIndex: Integer;
begin
  inherited;

  ThunderEffect := TThunderEffect.Create;

  FSpidersAppearing := TDynVector3SingleArray.Create;
  NextSpidersAppearingTime := 0;

  { TODO: this is not nice; I should add TLevelObject.Name for such
    purposes, and use here Objects.FindName('hint_button_box'). }
  HintOpenDoor := Objects.Items[0] as TLevelHintArea;

  FEndSequence := TLevelStaticObject.Create(Self,
    CastleLevelsPath + 'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    true { true: load background of EndSequence; we will use it });
  FEndSequence.Exists := false;
  { Even when FEndSequence will exist, we will not check for collisions
    with it --- no reason to waste time, no collisions will be possible
    as player's move along the EndSequence will be programmed. }
  FEndSequence.Collides := false;
  FEndSequence.CastsShadow := false; { shadow is not visible anyway }
  Objects.Add(FEndSequence);

  FGateExit := TLevelStaticObject.Create(Self,
    CastleLevelsPath + 'cages' + PathDelim + 'cages_gate_exit.wrl', false);
  FGateExit.CastsShadow := false; { shadow is not visible anyway }
  Objects.Add(FGateExit);

  BossIndex := Creatures.FindKind(SpiderQueen);
  if BossIndex <> -1 then
    FBossCreature := Creatures[BossIndex];
end;

destructor TCagesLevel.Destroy;
begin
  FreeAndNil(FSpidersAppearing);
  inherited;
end;

procedure TCagesLevel.SetDoEndSequence(Value: boolean);
begin
  { Changing from false to true ? Make sound. }
  if (not FDoEndSequence) and Value then
    SoundEngine.Sound(stKeyDoorUse);

  FDoEndSequence := Value;
  FEndSequence.Exists := DoEndSequence;
  FGateExit.Exists := not DoEndSequence;
end;

const
  { Remember to make it -1 lower than actual ceiling geometry,
    otherwise the spiders will be created on the ceiling of the model... }
  SpiderZ = 69.0;

procedure TCagesLevel.Idle(const CompSpeed: Single);
const
  { Some SpiderRadius is used to not put spider inside the wall. }
  SpiderRadius = 2;
  MinSpiderX = -11.0  + SpiderRadius;
  MaxSpiderX = 69.0   - SpiderRadius;
  MinSpiderY = -123.0 + SpiderRadius;
  MaxSpiderY = 162.0  - SpiderRadius;

  procedure AppearSpider(const Position: TVector3Single);
  begin
    FSpidersAppearing.AppendItem(Position);
  end;

  function RandomSpiderXY: TVector3Single;
  begin
    Result[0] := MapRange(Random, 0.0, 1.0, MinSpiderX, MaxSpiderX);
    Result[1] := MapRange(Random, 0.0, 1.0, MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

  function RandomSpiderXYAroundPlayer: TVector3Single;
  const
    RandomDist = 10.0;
  begin
    Result[0] := Player.Navigator.CameraPos[0] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[0] := Clamped(Result[0], MinSpiderX, MaxSpiderX);
    Result[1] := Player.Navigator.CameraPos[1] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[1] := Clamped(Result[1], MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

const
  SpidersFallingSpeed = 0.5;
  CreaturesCountToAddSpiders = 20;
var
  IsAboveTheGround: boolean;
  HeightAboveTheGround: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3Single;
  SpiderMoveDistance: Single;
  GroundItem: POctreeItem;
begin
  inherited;

  if not GameWin then
  begin
    { Torch light modify, to make an illusion of unstable light }
    LightSet.Lights.Items[0].LightNode.FdIntensity.Value := Clamped(
        LightSet.Lights.Items[0].LightNode.FdIntensity.Value +
          MapRange(Random, 0, 1, -0.1, 0.1) * CompSpeed,
        0.5, 1);
    LightSet.CalculateLights;

    { Maybe appear new spiders }
    if (Level.Creatures.Count < CreaturesCountToAddSpiders) and
       { Spider.PrepareRenderDone may be false here only if
         --debug-no-creatures was specified. In this case,
         leave Spider unprepared and don't use spider's on this level. }
       Spider.PrepareRenderDone then
    begin
      if NextSpidersAppearingTime = 0 then
      begin
        if AnimationTime > 1 then
        begin
          NextSpidersAppearingTime := AnimationTime + 5 + Random(20);
          for I := 1 to 5 + Random(3) do
            AppearSpider(RandomSpiderXY);
        end;
      end else
      if AnimationTime >= NextSpidersAppearingTime then
      begin
        NextSpidersAppearingTime := AnimationTime + 2 + Random(10);
        for I := 1 to 1 + Random(3) do
          AppearSpider(RandomSpiderXYAroundPlayer);
      end;
    end;

    { Move spiders down }
    I := 0;
    { 2 lines below only to get rid of compiler warnings }
    IsAboveTheGround := false;
    HeightAboveTheGround := 0;
    while I < FSpidersAppearing.Count do
    begin
      GetCameraHeight(FSpidersAppearing.Items[I], IsAboveTheGround,
        HeightAboveTheGround, GroundItem);
      if IsAboveTheGround and
        (HeightAboveTheGround < Spider.CameraRadius * 2) then
      begin
        SpiderPosition := FSpidersAppearing.Items[I];
        SpiderDirection :=
          VectorSubtract(Player.Navigator.CameraPos, SpiderPosition);
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, Level.GravityUp);
        SpiderCreature := Spider.CreateDefaultCreature(
          SpiderPosition, SpiderDirection, AnimationTime, Spider.DefaultMaxLife);
        Creatures.Add(SpiderCreature);
        SpiderCreature.Sound3d(stSpiderAppears, 1.0);
        FSpidersAppearing.Delete(I, 1);
      end else
      begin
        { calculate SpiderMoveDistance }
        SpiderMoveDistance := SpidersFallingSpeed * CompSpeed;
        { Actually IsAboveTheGround should be always @true, the way the
          "Cages" level is designed. However, I want to be safe here,
          in case someone will edit cages level, so I check IsAboveTheGround
          here. }
        if IsAboveTheGround then
          MinTo1st(SpiderMoveDistance,
            HeightAboveTheGround - Spider.CameraRadius);

        FSpidersAppearing.Items[I][2] -= SpiderMoveDistance;
        Inc(I);
      end;
    end;
  end else
    { No longer any need to show this hint. }
    HintOpenDoor.MessageDone := true;
end;

procedure TCagesLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword and 1 bow, to have weapons. }
  NewPlayer.PickItem(TItem.Create(Sword, 1));
  NewPlayer.PickItem(TItem.Create(Bow, 1));
end;

procedure TCagesLevel.Render(const Frustum: TFrustum);
var
  I: Integer;
begin
  { Render spiders before rendering inherited,
    because spiders are not transparent. }
  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
    glColorv(Black3Single);
    glBegin(GL_LINES);
      for I := 0 to FSpidersAppearing.High do
      begin
        glVertex3f(FSpidersAppearing.Items[I][0],
                   FSpidersAppearing.Items[I][1], SpiderZ);
        glVertexv(FSpidersAppearing.Items[I]);
      end;
    glEnd;
  glPopAttrib;

  for I := 0 to FSpidersAppearing.High do
  begin
    glPushMatrix;
      glTranslatev(FSpidersAppearing.Items[I]);
      Spider.StandAnimation.Scenes[0].Render(nil, tgAll);
    glPopMatrix;
  end;

  inherited;
end;

procedure TCagesLevel.RenderShadowVolume(
  ShadowVolumesHelper: TShadowVolumesHelper);
begin
  { TODO: render spiders shadow quads }
  inherited;
end;

procedure TCagesLevel.Picked(const Distance: Single;
  CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
begin
  inherited;

  if CollisionInfo.Hierarchy.IsLast(FGateExit) then
  begin
    InteractionOccured := true;
    if Distance > 10 then
      TimeMessageInteractFailed(
        'You see a door. You''re too far to open it from here') else
    begin
      if Player.Items.FindKind(RedKeyItemKind) <> -1 then
      begin
        if (BossCreature <> nil) and (not BossCreature.Dead) then
        begin
          Player.Knockback(2 + Random(5), 2, Vector3Single(0, -1, 0));
          SoundEngine.Sound(stEvilLaugh);
          TimeMessage('No exit for the one who does not fight');
        end else
        begin
          LevelFinished('');
          { As a price, the DOOM bonus level becomes available once
            you finish main game. }
          LevelsAvailable.FindName('doom_e1m1').AvailableForNewGame := true;
        end;
      end else
        TimeMessageInteractFailed('You need an appropriate key to open this door');
    end;
  end;
end;

function TCagesLevel.Background: TBackgroundGL;
begin
  if DoEndSequence then
    Result := FEndSequence.Scene.Background else
    Result := inherited;
end;

{ TDoomLevelDoor ------------------------------------------------------------- }

constructor TDoomLevelDoor.Create(AParentLevel: TLevel;
  const SceneFileName: string);
begin
  inherited Create(AParentLevel);
  MovingObject := TLevelStaticObject.Create(AParentLevel, SceneFileName, false);
  MovePushesOthers := false;
  SoundGoEndPosition := stDoorOpen;
  SoundGoBeginPosition := stDoorClose;
  CastsShadow := false; { looks bad }
end;

procedure TDoomLevelDoor.BeforeIdle(const NewAnimationTime: TAnimationTime);

  function SomethingWillBlockClosingDoor: boolean;
  var
    DoorBox: TBox3d;
    I: Integer;
  begin
    DoorBox := Box3dTranslate(
      (MovingObject as TLevelStaticObject).Scene.BoundingBox,
      Translation(NewAnimationTime));

    Result := Boxes3dCollision(DoorBox, Player.BoundingBox);
    if Result then
      Exit;

    for I := 0 to ParentLevel.Creatures.High do
    begin
      Result := Boxes3dCollision(DoorBox, ParentLevel.Creatures[I].BoundingBox);
      if Result then
        Exit;
    end;

    for I := 0 to ParentLevel.Items.High do
    begin
      Result := Boxes3dCollision(DoorBox, ParentLevel.Items[I].BoundingBox);
      if Result then
        Exit;
    end;
  end;

begin
  inherited;

  { First check the doors that are during closing:
    if the player or creatures will collide
    with them after AnimationTime will change,
    then we must stop and open again (to avoid
    entering into collision with player/creature because of
    door move). }

  if (not EndPosition) and
    (ParentLevel.AnimationTime - EndPositionStateChangeTime < MoveTime) and
    SomethingWillBlockClosingDoor then
    RevertGoEndPosition;
end;

procedure TDoomLevelDoor.Idle;
begin
  inherited;

  if EndPosition and
    (ParentLevel.AnimationTime - EndPositionStateChangeTime >
      MoveTime + StayOpenTime) then
    GoBeginPosition;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  DoomDoorsPathPrefix: string;

  function MakeDoor(const FileName: string): TDoomLevelDoor;
  begin
    Result := TDoomLevelDoor.Create(Self, DoomDoorsPathPrefix + FileName);

    { Although I didn't know it initially, it turns out that all doors
      on Doom E1M1 level (maybe all doors totally ?) have the same
      values for parameters below. }
    Result.MoveTime := 1.0;
    Result.TranslationEnd.Init(0, 0, 3.5);
    Result.StayOpenTime := 5.0;
  end;

begin
  inherited;

  DoomDoorsPathPrefix := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' +
    PathDelim;

  Objects.Add(MakeDoor('door2_3_closed.wrl'));
  Objects.Add(MakeDoor('door4_5_closed.wrl'));
  Objects.Add(MakeDoor('door4_7_closed.wrl'));
  Objects.Add(MakeDoor('door5_6_closed.wrl'));

  FakeWall := TLevelStaticObject.Create(Self,
    DoomDoorsPathPrefix + 'fake_wall_final.wrl', false);
  { Although TLevelStaticObject will create useless triangle octree
    for FakeWall, it's not a problem: FakeWall is very small, so it's
    octree will be generated very fast and will be very small in memory.
    Not a problem to worry. }
  FakeWall.Collides := false;
  FakeWall.CastsShadow := false;
  Objects.Add(FakeWall);

  Elevator49 := TLevelStaticObject.Create(
    Self, DoomDoorsPathPrefix + 'elevator4_9_final.wrl', false);

  MovingElevator49 := TLevelLinearMovingObject.Create(Self);
  MovingElevator49.MovingObject := Elevator49;
  MovingElevator49.MoveTime := 3.0;
  MovingElevator49.TranslationEnd.Init(0, 0, -6.7);
  MovingElevator49.SoundGoEndPosition := stElevator;
  MovingElevator49.SoundGoEndPositionLooping := true;
  MovingElevator49.SoundGoBeginPosition := stElevator;
  MovingElevator49.SoundGoBeginPositionLooping := true;
  MovingElevator49.SoundTracksCurrentPosition := true;
  MovingElevator49.CastsShadow := false;
  Objects.Add(MovingElevator49);

  Elevator9a9b := TLevelStaticObject.Create(
    Self, DoomDoorsPathPrefix + 'elevator_9a_9b_final.wrl', false);

  MovingElevator9a9b := TLevelLinearMovingObject.Create(Self);
  MovingElevator9a9b.MovingObject := Elevator9a9b;
  MovingElevator9a9b.MoveTime := 3.0;
  MovingElevator9a9b.TranslationEnd.Init(0, 0, -7.5);
  MovingElevator9a9b.SoundGoEndPosition := stElevator;
  MovingElevator9a9b.SoundGoEndPositionLooping := true;
  MovingElevator9a9b.SoundGoBeginPosition := stElevator;
  MovingElevator9a9b.SoundGoBeginPositionLooping := true;
  MovingElevator9a9b.SoundTracksCurrentPosition := true;
  MovingElevator9a9b.CastsShadow := false;
  Objects.Add(MovingElevator9a9b);

  ExitButton := TLevelStaticObject.Create(Self,
    DoomDoorsPathPrefix + 'exit_button_final.wrl', false);
  ExitButton.CastsShadow := false;
  Objects.Add(ExitButton);
end;

destructor TDoomE1M1Level.Destroy;
begin
  inherited;
end;

procedure TDoomE1M1Level.Picked(const Distance: Single;
  CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
  var InteractionOccured: boolean);
var
  Door: TDoomLevelDoor;
begin
  inherited;

  if (CollisionInfo.Hierarchy.Count > 0) and
    (CollisionInfo.Hierarchy.First is TDoomLevelDoor) then
  begin
    Door := TDoomLevelDoor(CollisionInfo.Hierarchy.First);
    InteractionOccured := true;
    if Distance > 7 then
      TimeMessageInteractFailed('You see a door. You''re too far to open it from here') else
    { Only if the door is completely closed
      (and not during closing right now) we allow player to open it. }
    if not Door.CompletelyBeginPosition then
      TimeMessageInteractFailed('You see a door. It''s already open') else
      Door.GoEndPosition;
  end else
  if CollisionInfo.Hierarchy.IsLast(Elevator9a9b) and
     MovingElevator9a9b.CompletelyBeginPosition and
     Box3dPointInside(Player.Navigator.CameraPos, Elevator9a9bPickBox) then
  begin
    InteractionOccured := true;
    if Distance > 10 then
      TimeMessageInteractFailed(
        'You''re too far to reach it from here') else
      MovingElevator9a9b.GoEndPosition;
  end else
  if CollisionInfo.Hierarchy.IsLast(ExitButton) then
  begin
    InteractionOccured := true;
    if Distance > 5 then
      TimeMessageInteractFailed(
        'You''re too far to reach it from here') else
      begin
        SoundEngine.Sound(stDoomExitButton);
        Player.Life := 0;
        ExitMessagePending := true;
      end;
  end;
end;

procedure TDoomE1M1Level.RenameCreatures(Node: TVRMLNode);
const
  SCreaDoomZomb = 'CreaDoomZomb_';
  SCreaDoomSerg = 'CreaDoomSerg_';
begin
  { This is just a trick to rename all creatures 'DoomZomb' and 'DoomSerg'
    on level just to our 'Alien' creature. In the future maybe we will
    have real (and different) DoomZomb/Serg creatures, then the trick
    below will be removed. }
  if IsPrefix(SCreaDoomZomb, Node.NodeName) then
    Node.NodeName := 'CreaAlien_' + SEnding(Node.NodeName, Length(SCreaDoomZomb) + 1) else
  if IsPrefix(SCreaDoomSerg, Node.NodeName) then
    Node.NodeName := 'CreaAlien_' + SEnding(Node.NodeName, Length(SCreaDoomSerg) + 1);
end;

procedure TDoomE1M1Level.ChangeLevelScene;
begin
  inherited;

  Scene.RootNode.EnumerateNodes(@RenameCreatures, true);
  RemoveBoxNodeCheck(Elevator49DownBox, 'Elevator49DownBox');
  RemoveBoxNodeCheck(Elevator9a9bPickBox, 'Elev9a9bPickBox');
end;

procedure TDoomE1M1Level.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  NewPlayer.PickItem(TItem.Create(Bow, 1));
  NewPlayer.PickItem(TItem.Create(Quiver, 10));
end;

procedure TDoomE1M1Level.Idle(const CompSpeed: Single);
begin
  inherited;

  if MovingElevator49.CompletelyBeginPosition and
     Box3dPointInside(Player.Navigator.CameraPos, Elevator49DownBox) then
  begin
    MovingElevator49.GoEndPosition;
  end;

  if MovingElevator9a9b.CompletelyEndPosition and
     (AnimationTime - MovingElevator9a9b.EndPositionStateChangeTime >
       MovingElevator9a9b.MoveTime +
       { This is the time for staying in lowered position. }
       2.0) then
    MovingElevator9a9b.GoBeginPosition;

  if ExitMessagePending and (not Player.Navigator.FallingOnTheGround) then
  begin
    { ExitMessagePending is displayed when player FallOnTheGround effect
      (when dying) ended. }
    MessageOK(Glw,
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
      '[http://vrmlengine.sourceforge.net/castle.php]. ' +
      'Oh, and this is open-source game, so if you can, ' +
      'you''re most welcome to contribute!', taLeft);
    ExitMessagePending := false;
  end;
end;

{ TGateDemoLevel ------------------------------------------------------------- }

constructor TGateDemoLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ADemo: boolean);
var
  Water: TLevelSimpleAnimatedObject;
begin
  inherited;

  Water := TLevelSimpleAnimatedObject.Create(Self,
    LoadLevelAnimation(CastleLevelsPath + 'gate_demo' +
      PathDelim + 'water.kanim', false, false));
  Water.CastsShadow := false; { water shadow would look awkward }
  { No octrees created for water (because in normal usage, player will not
    walk on this level). For safety, Collides set to @false, in case
    user enters this level by debug menu. }
  Water.Collides := false;
  Objects.Add(Water);

  Water.Play;
end;

end.
