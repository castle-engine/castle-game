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
  ALSourceAllocator, CastleSound, Matrix, VRMLNodes, DOM;

type
  TCastleHallLevel = class(TLevel)
  private
    Symbol: TLevelSimpleAnimatedObject;
    Button: TLevelSimpleAnimatedObject;
    HintButton: TLevelHintArea;

    StairsBlocker: TLevelStaticObject;

    FLevelExitBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ANumber: Integer;
      DOMElement: TDOMElement); override;

    procedure Idle(const CompSpeed: Single); override;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure DestroyStairsBlocker;
  end;

  TGateLevel = class(TLevel)
  private
    FGateExitBox: TBox3d;

    TeleportOpaque, TeleportTransp: TVRMLFlatSceneGL;
    FTeleport1Box, FTeleport2Box: TBox3d;

    Teleport1Rotate: Single;
    Teleport2Rotate: Single;

    Teleport1Destination: TVector3Single;
    Teleport2Destination: TVector3Single;

    SacrilegeAmbushStartingPosition: array[0..5]of TVector3Single;
    SwordAmbushStartingPosition: array[0..2]of TVector3Single;

    SacrilegeAmbushDone: boolean;
    SwordAmbushDone: boolean;

    FSacrilegeBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ANumber: Integer;
      DOMElement: TDOMElement); override;
    destructor Destroy; override;

    function CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single); override;

    procedure Render(const Frustum: TFrustum); override;
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
      const ATitle: string; const ANumber: Integer;
      DOMElement: TDOMElement); override;

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
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ANumber: Integer;
      DOMElement: TDOMElement); override;
    destructor Destroy; override;

    procedure Idle(const CompSpeed: Single); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Render(const Frustum: TFrustum); override;

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

    procedure BeforeIdle(const NewAnimationTime: Single); override;
    procedure Idle; override;

    property MovePushesOthers default false;
    property SoundGoBeginPosition default stDoorClose;
    property SoundGoEndPosition default stDoorOpen;
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TVRMLNode);

    HintOpenDoor: TLevelHintArea;

    FakeWall: TLevelStaticObject;

    Elevator49: TVRMLFlatSceneGL;
    Elevator49Down: boolean;
    Elevator49DownBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName, ALightSetFileName: string;
      const ATitle: string; const ANumber: Integer;
      DOMElement: TDOMElement); override;
    destructor Destroy; override;

    procedure Picked(const Distance: Single;
      CollisionInfo: TCollisionInfo; LevelObjectIndex: Integer;
      var InteractionOccured: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

function CastleLevelsPath: string;

implementation

uses KambiFilesUtils, SysUtils, Object3dAsVRML, KambiUtils,
  OpenGLh, KambiGLUtils, KambiStringUtils,
  CastleCreatures, CastlePlay, CastleTimeMessages, CastleInputs,
  CastleItems, CastleThunder, CastleLevelAvailable;

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ANumber: Integer;
  DOMElement: TDOMElement);
var
  CastleHallLevelPath: string;
begin
  inherited;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := TLevelSimpleAnimatedObject.Create(Self,
    LoadLevelAnimation(CastleHallLevelPath + 'symbol.kanim', true));
  Objects.Add(Symbol);

  Button := TLevelSimpleAnimatedObject.Create(Self,
    LoadLevelAnimation(CastleHallLevelPath + 'button.kanim', true));
  Objects.Add(Button);

  StairsBlocker := TLevelStaticObject.Create(Self,
    CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl', false);
  Objects.Add(StairsBlocker);
end;

procedure TCastleHallLevel.ChangeLevelScene;
begin
  HintButton := TLevelHintArea.Create(Self);
  HintButton.VRMLName := 'HintButtonBox';
  HintButton.Message := 'Hint: press this red button with the %i';
  Objects.Add(HintButton);

  inherited;

  RemoveBoxNodeCheck(FLevelExitBox, 'LevelExitBox');
end;

procedure TCastleHallLevel.Idle(const CompSpeed: Single);
const
  WerewolfStartPosition: TVector3Single = (0, 0, -4);
var
  WerewolfCreature: TCreature;
begin
  inherited;

  if Box3dPointInside(Player.Navigator.CameraPos, FLevelExitBox) then
  begin
    LevelFinished(LevelsAvailable.FindName('cages').CreateLevel);
  end;

  if Button.Started and
    (AnimationTime - Button.PlayStartTime > Button.Animation.TimeDuration) then
  begin
    if not Symbol.Started then
    begin
      Symbol.Play;
      Symbol.Collides := false;

      WerewolfCreature := Werewolf.CreateDefaultCreature(
        WerewolfStartPosition,
        Vector3Single(0, 1, 0), AnimationTime, Werewolf.DefaultMaxLife);
      Creatures.Add(WerewolfCreature);
      WerewolfCreature.Sound3d(stWerewolfHowling, 1.0);

      FBossCreature := WerewolfCreature;

      Sound3d(stCastleHallSymbolMoving, Vector3Single(0, 0, 0));
    end;
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

procedure TCastleHallLevel.DestroyStairsBlocker;
begin
  if StairsBlocker.Exists then
  begin
    StairsBlocker.Exists := false;
    Sound3d(stStairsBlockerDestroyed, Box3dMiddle(StairsBlocker.Scene.BoundingBox));
  end;
end;

{ TGateLevel ----------------------------------------------------------------- }

constructor TGateLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ANumber: Integer;
  DOMElement: TDOMElement);
begin
  inherited;

  TeleportOpaque := LoadLevelScene(
    CastleLevelsPath + 'gate' + PathDelim + 'teleport_opaque.wrl',
    false, false);

  TeleportTransp := LoadLevelScene(
    CastleLevelsPath + 'gate' + PathDelim + 'teleport_transp.wrl',
    false, false);

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
end;

destructor TGateLevel.Destroy;
begin
  FreeAndNil(TeleportOpaque);
  FreeAndNil(TeleportTransp);
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

      Sound(stTeleport);
    end;
  end;

  procedure SacrilegeAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
    Creature: TCreature;
  begin
    Sound(stSacrilegeAmbush);
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
      LevelFinished(LevelsAvailable.FindName('castle_hall').CreateLevel);
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
      (Player.Items.FindKind(Sword) <> -1) then
    begin
      SwordAmbushDone := true;
      SwordAmbush;
    end;
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
    TeleportScene: TVRMLFlatSceneGL);
  begin
    if FrustumBox3dCollisionPossibleSimple(Frustum, TeleportBox) then
    begin
      glPushMatrix;
        glTranslatev(Box3dMiddle(TeleportBox));
        glRotatef(TeleportRotation, 1, 1, 0);
        TeleportScene.Render(nil);
      glPopMatrix;
    end;
  end;

begin
  RenderTeleport(Teleport1Rotate, FTeleport1Box, TeleportOpaque);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, TeleportOpaque);
  inherited;
  RenderTeleport(Teleport1Rotate, FTeleport1Box, TeleportTransp);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, TeleportTransp);
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create(
  const AName: string;
  const ASceneFileName, ALightSetFileName: string;
  const ATitle: string; const ANumber: Integer;
  DOMElement: TDOMElement);
var
  ElevatorButtonSum: TLevelObjectSum;
  TowerLevelPath: string;
begin
  inherited;

  TowerLevelPath := CastleLevelsPath + 'tower' + PathDelim;

  Elevator := TLevelStaticObject.Create(
    Self, TowerLevelPath + 'elevator.wrl', false);

  ElevatorButton := TLevelSimpleAnimatedObject.Create(Self, LoadLevelAnimation(
    TowerLevelPath + 'elevator_button.kanim', true));

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
  const ATitle: string; const ANumber: Integer;
  DOMElement: TDOMElement);
var
  BossIndex: Integer;
begin
  inherited;

  ThunderEffect := TThunderEffect.Create;

  FSpidersAppearing := TDynVector3SingleArray.Create;
  NextSpidersAppearingTime := 0;

  FEndSequence := TLevelStaticObject.Create(Self,
    CastleLevelsPath + 'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    true { true: load background of EndSequence; we will use it });
  FEndSequence.Exists := false;
  { Even when FEndSequence will exist, we will not check for collisions
    with it --- no reason to waste time, no collisions will be possible
    as player's move along the EndSequence will be programmed. }
  FEndSequence.Collides := false;
  { Actually, FEndSequence is not transparent as a whole. But part of it
    is transparent, so it must be rendered after main level scene.
    Fortunately, this is our only transparent object, so it will be rendered
    *right after* main level geometry, which means that both it's non-transparent
    and transparent parts will render correctly. }
  FEndSequence.Transparent := true;
  Objects.Add(FEndSequence);

  FGateExit := TLevelStaticObject.Create(Self,
    CastleLevelsPath + 'cages' + PathDelim + 'cages_gate_exit.wrl', false);
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
  FDoEndSequence := Value;
  FEndSequence.Exists := DoEndSequence;
  FGateExit.Exists := not DoEndSequence;
end;

procedure TCagesLevel.ChangeLevelScene;
begin
  HintOpenDoor := TLevelHintArea.Create(Self);
  HintOpenDoor.VRMLName := 'HintOpenDoorBox';
  HintOpenDoor.Message := 'Hint: open this door using the %i';
  Objects.Add(HintOpenDoor);

  inherited;
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
  SqrHeightAboveTheGround: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3Single;
  SpiderMoveDistance: Single;
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
       (not WasParam_DebugNoCreatures) then
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
    SqrHeightAboveTheGround := 0;
    while I < FSpidersAppearing.Count do
    begin
      GetCameraHeight(FSpidersAppearing.Items[I], IsAboveTheGround,
        SqrHeightAboveTheGround);
      if IsAboveTheGround and
        (SqrHeightAboveTheGround < Sqr(Spider.CameraRadius * 2)) then
      begin
        SpiderPosition := FSpidersAppearing.Items[I];
        SpiderDirection :=
          VectorSubtract(Player.Navigator.CameraPos, SpiderPosition);
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, Level.HomeCameraUp);
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
            Sqrt(SqrHeightAboveTheGround) - Spider.CameraRadius);

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
      Spider.StandAnimation.Scenes[0].Render(nil);
    glPopMatrix;
  end;

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
          Sound(stEvilLaugh);
          TimeMessage('No exit for the one who does not fight');
        end else
          LevelFinished(nil);
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
end;

procedure TDoomLevelDoor.BeforeIdle(const NewAnimationTime: Single);

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
  const ATitle: string; const ANumber: Integer;
  DOMElement: TDOMElement);
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

  {TODO: use elevator}

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
  Objects.Add(FakeWall);

  Elevator49 := LoadLevelScene(DoomDoorsPathPrefix + 'elevator4_9_final.wrl',
    true, false);
end;

destructor TDoomE1M1Level.Destroy;
begin
  FreeAndNil(Elevator49);

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
  HintOpenDoor := TLevelHintArea.Create(Self);
  HintOpenDoor.VRMLName := 'HintOpenDoorBox';
  HintOpenDoor.Message := 'Hint: open doors using the %i';
  Objects.Add(HintOpenDoor);

  inherited;

  Scene.RootNode.EnumerateNodes(@RenameCreatures, true);
  RemoveBoxNodeCheck(Elevator49DownBox, 'Elevator49DownBox');
end;

procedure TDoomE1M1Level.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  NewPlayer.PickItem(TItem.Create(Bow, 1));
  NewPlayer.PickItem(TItem.Create(Quiver, 10));
end;

end.