{ TLevel specialized descendants. }
unit CastleLevelSpecific;

interface

uses VRMLGLAnimation, VRMLFlatSceneGL, Boxes3d, VectorMath,
  CastlePlayer, CastleLevel, VRMLTriangleOctree, BackgroundGL,
  ALSourceAllocator, CastleSound, Matrix, VRMLNodes;

type
  TCastleHallLevel = class(TLevel)
  private
    SymbolOpen: boolean;
    SymbolOpenTime: Single;
    Symbol: TVRMLGLAnimation;

    ButtonPressed: boolean;
    ButtonPressedTime: Single;
    Button: TVRMLGLAnimation;

    { HintButtonBox, indicating when to display hint about pressing a button. }
    FHintButtonBox: TBox3d;
    FHintButtonShown: boolean;

    StairsBlocker: TLevelStaticObject;

    { Check collision only with Symbol, Button
      --- but not with real level and level objects geometry
      (i.e. not with things handled by inherited
      MoveAllowed, MoveAllowedSimple). }
    function MoveAllowedAdditionalSimple(
      const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean;

    FLevelExitBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    function LineOfSight(const Pos1, Pos2: TVector3Single): boolean;
      override;

    function MoveAllowed(const CameraPos: TVector3Single;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    function MoveAllowedSimple(const CameraPos: TVector3Single;
      const NewPos: TVector3Single;
      const BecauseOfGravity: boolean;
      const MovingObjectCameraRadius: Single): boolean; override;

    procedure GetCameraHeight(const CameraPos: TVector3Single;
      out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);
      override;

    procedure Render(const Frustum: TFrustum); override;
    procedure Idle(const CompSpeed: Single); override;

    function SpecialObjectsTryPick(var IntersectionDistance: Single;
      const Ray0, RayVector: TVector3Single): Integer; override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;

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
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    function CollisionIgnoreItem(Octree: TVRMLTriangleOctree;
      OctreeItemIndex: Integer): boolean; override;
    procedure Idle(const CompSpeed: Single); override;

    procedure Render(const Frustum: TFrustum); override;
  end;

  TTowerLevel = class(TLevel)
  public
    constructor Create; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;
  end;

  TCagesLevel = class(TLevel)
  private
    FSpidersAppearing: TDynVector3SingleArray;
    NextSpidersAppearingTime: Single;

    FHintOpenDoorBox: TBox3d;
    HintOpenDoorBoxShown: boolean;

    FGateExit: TLevelStaticObject;

    FDoEndSequence: boolean;

    FEndSequence: TVRMLFlatSceneGL;
    procedure SetDoEndSequence(Value: boolean);
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    procedure Idle(const CompSpeed: Single); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Render(const Frustum: TFrustum); override;

    property EndSequence: TVRMLFlatSceneGL read FEndSequence;

    { True means that GateExit will not be rendered (or collided)
      and EndSequence will be rendered. }
    property DoEndSequence: boolean
      read FDoEndSequence write SetDoEndSequence default false;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;

    function Background: TBackgroundGL; override;
  end;

  TDoomLevelDoor = class(TLevelMovingObject)
  private
    UsedSound: TALAllocatedSource;
    procedure SoundSourceUsingEnd(Sender: TALAllocatedSource);
    procedure PlaySound(SoundType: TSoundType);
  public
    constructor Create(AParentLevel: TLevel;
      const SceneFileName: string);
    destructor Destroy; override;

    { Constant fields, i.e. once set in TDoomE1M1Level constructor.
      Scene should have door placed in closed position.
      @groupBegin }
    OpenCloseTime: Single;
    OpenMove: TVector3_Single;
    StayOpenTime: Single;
    { @groupEnd }

    { Variable fields. They may change during level lifetime.
      They are initialized automatically in level constructor
      from constant fields above.
      @groupBegin }
    Open: boolean;
    OpenStateChangeTime: Single;
    { @groupEnd }

    procedure DoOpen;
    procedure DoClose;
    procedure RevertDoOpen;

    function SceneTranslation(const AnimationTime: Single):
      TVector3_Single; override;

    function CompletelyOpen: boolean;
    function CompletelyClosed: boolean;

    procedure Render(const Frustum: TFrustum;
      const ATransparent: boolean); override;
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TVRMLNode);

    FHintOpenDoorBox: TBox3d;
    HintOpenDoorShown: boolean;

    FakeWall: TLevelStaticObject;

    Elevator49: TVRMLFlatSceneGL;
    Elevator49Down: boolean;
    Elevator49DownBox: TBox3d;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function SceneFileName: string; override;
    class function LightSetFileName: string; override;

    class function Title: string; override;
    class function Number: Integer; override;

    procedure Idle(const CompSpeed: Single); override;

    procedure SpecialObjectPicked(const Distance: Single;
      SpecialObjectIndex: Integer); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

implementation

uses KambiFilesUtils, SysUtils, Object3dAsVRML, KambiUtils,
  OpenGLh, KambiGLUtils, KambiStringUtils,
  CastleCreatures, CastlePlay, CastleTimeMessages, CastleKeys,
  CastleItems, CastleThunder;

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create;
var
  CastleHallLevelPath: string;
begin
  inherited;

  PlayedMusicSound := stCastleHallMusic;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := LoadLevelAnimation(
    [ LoadAsVRML(CastleHallLevelPath + 'castle_hall_symbol_closed.wrl'),
      LoadAsVRML(CastleHallLevelPath + 'castle_hall_symbol_open.wrl') ],
    true, [ 0, 1.5 ], 30, roSeparateShapeStatesNoTransform, 0.1, false, false,
    true);

  Button := LoadLevelAnimation(
    [ LoadAsVRML(CastleHallLevelPath + 'castle_hall_button_not_pressed.wrl'),
      LoadAsVRML(CastleHallLevelPath + 'castle_hall_button_pressed.wrl') ],
    true, [ 0, 0.5 ], 30, roSeparateShapeStatesNoTransform, 0.01, false, false,
    true);

  StairsBlocker := TLevelStaticObject.Create(Self,
    CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl', false);
  Objects.Add(StairsBlocker);

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;
end;

destructor TCastleHallLevel.Destroy;
begin
  FreeAndNil(Button);
  FreeAndNil(Symbol);
  inherited;
end;

procedure TCastleHallLevel.ChangeLevelScene;
begin
  inherited;
  RemoveBoxNodeCheck(FLevelExitBox, 'LevelExitBox');
  RemoveBoxNodeCheck(FHintButtonBox, 'HintButtonBox');
end;

class function TCastleHallLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_final.wrl';
end;

class function TCastleHallLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'castle_hall' + PathDelim + 'castle_hall_lights.wrl';
end;

class function TCastleHallLevel.Title: string;
begin
  Result := 'Castle Hall';
end;

class function TCastleHallLevel.Number: Integer;
begin
  Result := 2;
end;

function TCastleHallLevel.LineOfSight(
  const Pos1, Pos2: TVector3Single): boolean;
begin
  Result := inherited;

  Result := Result and
    (Button.FirstScene.DefaultTriangleOctree.SegmentCollision(
      Pos1, Pos2, false, NoItemIndex, false,
      @Button.FirstScene.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex);

  if Result and (not SymbolOpen) then
  begin
    Result :=
      Symbol.FirstScene.DefaultTriangleOctree.SegmentCollision(
        Pos1, Pos2, false, NoItemIndex, false,
        @Symbol.FirstScene.DefaultTriangleOctree.IgnoreTransparentItem) = NoItemIndex;
  end;
end;

function TCastleHallLevel.MoveAllowedAdditionalSimple(
  const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result :=
    Button.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
      CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);

  if Result and (not SymbolOpen) then
  begin
    Result :=
      Symbol.FirstScene.DefaultTriangleOctree.MoveAllowedSimple(
        CameraPos, NewPos, MovingObjectCameraRadius, NoItemIndex, nil);
  end;
end;

function TCastleHallLevel.MoveAllowed(const CameraPos: TVector3Single;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  Result := Result and MoveAllowedAdditionalSimple(
    CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

function TCastleHallLevel.MoveAllowedSimple(const CameraPos: TVector3Single;
  const NewPos: TVector3Single;
  const BecauseOfGravity: boolean;
  const MovingObjectCameraRadius: Single): boolean;
begin
  Result := inherited;

  Result := Result and MoveAllowedAdditionalSimple(
    CameraPos, NewPos, BecauseOfGravity, MovingObjectCameraRadius);
end;

procedure TCastleHallLevel.GetCameraHeight(const CameraPos: TVector3Single;
  out IsAboveTheGround: boolean; out SqrHeightAboveTheGround: Single);

  procedure MakeBonusScene(BonusScene: TVRMLFlatSceneGL);
  var
    IsAboveTheBonusScene: boolean;
    SqrHeightAboveTheBonusScene: Single;
  begin
    BonusScene.DefaultTriangleOctree.GetCameraHeight(
      CameraPos, HomeCameraUp,
      IsAboveTheBonusScene, SqrHeightAboveTheBonusScene,
      NoItemIndex, nil);

    if IsAboveTheBonusScene then
    begin
      if not IsAboveTheGround then
      begin
        IsAboveTheGround := IsAboveTheBonusScene;
        SqrHeightAboveTheGround := SqrHeightAboveTheBonusScene;
      end else
        SqrHeightAboveTheGround :=
          Min(SqrHeightAboveTheGround, SqrHeightAboveTheBonusScene);
    end;
  end;

begin
  inherited GetCameraHeight(CameraPos, IsAboveTheGround, SqrHeightAboveTheGround);

  if not SymbolOpen then
    MakeBonusScene(Symbol.FirstScene);

  MakeBonusScene(Button.FirstScene);
end;

procedure TCastleHallLevel.Render(const Frustum: TFrustum);
begin
  if SymbolOpen then
    Symbol.SceneFromTime(AnimationTime - SymbolOpenTime).Render(nil) else
    Symbol.SceneFromTime(0).RenderFrustum(Frustum);

  if ButtonPressed then
    Button.SceneFromTime(AnimationTime - ButtonPressedTime).Render(nil) else
    Button.SceneFromTime(0).RenderFrustum(Frustum);

  { Note that we render Symbol before inherited, i.e. before rendering
    real level --- to allow alpha objects on level to be rendered as last. }
  inherited;
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
    LevelFinished(TCagesLevel.Create);
  end;

  if ButtonPressed and
    (AnimationTime - ButtonPressedTime > Button.TimeDuration) then
  begin
    if not SymbolOpen then
    begin
      SymbolOpen := true;
      SymbolOpenTime := AnimationTime;

      WerewolfCreature := Werewolf.CreateDefaultCreature(
        WerewolfStartPosition,
        Vector3Single(0, 1, 0), AnimationTime, Werewolf.DefaultMaxLife);
      Creatures.Add(WerewolfCreature);
      WerewolfCreature.Sound3d(stWerewolfHowling, 1.0);

      FBossCreature := WerewolfCreature;

      Sound3d(stCastleHallSymbolMoving, Vector3Single(0, 0, 0));
    end;
  end;

  if (not FHintButtonShown) and
     Box3dPointInside(Player.Navigator.CameraPos, FHintButtonBox) then
  begin
    TimeMessage('Hint: press this red button with the ' +
      InteractKeyDescription);
    FHintButtonShown := true;
  end;
end;

function TCastleHallLevel.SpecialObjectsTryPick(var IntersectionDistance: Single;
  const Ray0, RayVector: TVector3Single): Integer;

  procedure MakeBonusScene(Scene: TVRMLFlatSceneGL; SpecialObjectIndex: Integer);
  var
    ThisIntersectionDistance: Single;
  begin
    if (Scene.DefaultTriangleOctree.RayCollision(
      ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex,
      false, nil) <> NoItemIndex) and
      ( (Result = -1) or
        (ThisIntersectionDistance < IntersectionDistance) ) then
    begin
      IntersectionDistance := ThisIntersectionDistance;
      Result := SpecialObjectIndex;
    end;
  end;

begin
  Result := inherited SpecialObjectsTryPick(
    IntersectionDistance, Ray0, RayVector);

  MakeBonusScene(Button.FirstScene, 1);
end;

procedure TCastleHallLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
begin
  inherited;

  case SpecialObjectIndex of
    0:begin
        TimeMessage('You are not able to open it');
        Sound(stPlayerInteractFailed);
      end;
    1:
      begin
        if Distance < 10.0 then
        begin
          if ButtonPressed then
            TimeMessage('Button is already pressed') else
          begin
            ButtonPressed := true;
            ButtonPressedTime := AnimationTime;
            TimeMessage('You press the button');
          end;
        end else
          TimeMessage('You see a button. You cannot reach it from here');
      end;
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

constructor TGateLevel.Create;
begin
  inherited;

  PlayedMusicSound := stGateMusic;

  FFootstepsSound := stPlayerFootstepsGrass;

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

class function TGateLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'gate' + PathDelim + 'gate_final.wrl';
end;

class function TGateLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'gate' + PathDelim + 'gate_lights.wrl';
end;

class function TGateLevel.Title: string;
begin
  Result := 'The Gate';
end;

class function TGateLevel.Number: Integer;
begin
  Result := 1;
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
      LevelFinished(TCastleHallLevel.Create);
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

constructor TTowerLevel.Create;
begin
  inherited;

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;
end;

class function TTowerLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_final.wrl';
end;

class function TTowerLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'tower' + PathDelim + 'basic_castle_lights.wrl';
end;

class function TTowerLevel.Title: string;
begin
  Result := 'Tower';
end;

class function TTowerLevel.Number: Integer;
begin
  Result := 99;
end;

{ TCagesLevel ---------------------------------------------------------------- }

constructor TCagesLevel.Create;
var
  BossIndex: Integer;
begin
  inherited;

  PlayedMusicSound := stCagesMusic;

  ThunderEffect := TThunderEffect.Create;

  if Headlight <> nil then
  begin
    Headlight.DiffuseColor := Vector4Single(1, 1, 0.29, 1.0);
    Headlight.SpecularColor := Vector4Single(1, 1, 0.29, 1.0);
    Headlight.Spot := true;
//    Headlight.Attenuation := Vector3Single(1, 0.1, 0);
    Headlight.SpotCutoff := 40;
    Headlight.SpotExponent := 0.1;
  end;

  FSpidersAppearing := TDynVector3SingleArray.Create;
  NextSpidersAppearingTime := 0;

  HintOpenDoorBoxShown := false;

  FEndSequence := LoadLevelScene(CastleLevelsPath +
    'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    false, true);

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
  FreeAndNil(FEndSequence);
  inherited;
end;

procedure TCagesLevel.SetDoEndSequence(Value: boolean);
begin
  FDoEndSequence := Value;
  FGateExit.Exists := not DoEndSequence;
end;

procedure TCagesLevel.ChangeLevelScene;
begin
  inherited;

  RemoveBoxNodeCheck(FHintOpenDoorBox, 'HintOpenDoorBox');
end;

class function TCagesLevel.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'cages' + PathDelim + 'cages_final.wrl';
end;

class function TCagesLevel.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'cages' + PathDelim + 'cages_lights.wrl';
end;

class function TCagesLevel.Title: string;
begin
  Result := 'Cages';
end;

class function TCagesLevel.Number: Integer;
begin
  Result := 3;
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

    if (not HintOpenDoorBoxShown) and
      Box3dPointInside(Player.Navigator.CameraPos, FHintOpenDoorBox) then
    begin
      HintOpenDoorBoxShown := true;
      TimeMessage('Hint: open this door using the ' +
        InteractKeyDescription);
    end;
  end;
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

  if DoEndSequence then
    EndSequence.RenderFrustum(Frustum);
end;

procedure TCagesLevel.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
var
  SpiderQueenIndex: Integer;
begin
  inherited;

  case SpecialObjectIndex of
    0:begin
        if Distance > 10 then
          TimeMessage('You see a door. You''re too far to open it from here') else
        begin
          if Player.Items.FindKind(RedKeyItemKind) <> -1 then
          begin
            SpiderQueenIndex := Creatures.FindKind(SpiderQueen);
            if (SpiderQueenIndex <> -1) and
              (not Creatures[SpiderQueenIndex].Dead) then
            begin
              Player.Knockback(2 + Random(5), 2, Vector3Single(0, -1, 0));
              Sound(stEvilLaugh);
              TimeMessage('No exit for the one who does not fight');
            end else
              LevelFinished(nil);
          end else
            TimeMessage('You need an appropriate key to open this door');
        end;
      end;
  end;
end;

function TCagesLevel.Background: TBackgroundGL;
begin
  if DoEndSequence then
    Result := EndSequence.Background else
    Result := inherited;
end;

{ TDoomLevelDoor ------------------------------------------------------------- }

constructor TDoomLevelDoor.Create(AParentLevel: TLevel;
  const SceneFileName: string);
begin
  inherited Create(AParentLevel, SceneFileName, false);
end;

destructor TDoomLevelDoor.Destroy;
begin
  if UsedSound <> nil then
  begin
    { We detach ourselved from UsedSound, but we let UsedSound to eventually
      continue playing. }
    UsedSound.OnUsingEnd := nil;
    UsedSound := nil;
  end;

  inherited;
end;

procedure TDoomLevelDoor.SoundSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = UsedSound);
  UsedSound.OnUsingEnd := nil;
  UsedSound := nil;
end;

procedure TDoomLevelDoor.PlaySound(SoundType: TSoundType);
begin
  { Door can play only one sound at a time. }
  if UsedSound <> nil then
    UsedSound.DoUsingEnd;
  UsedSound := Sound3d(SoundType, Box3dMiddle(Scene.BoundingBox));
  if UsedSound <> nil then
    UsedSound.OnUsingEnd := @SoundSourceUsingEnd;
end;

procedure TDoomLevelDoor.DoOpen;
begin
  Open := true;
  OpenStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(stDoorOpen);
end;

procedure TDoomLevelDoor.DoClose;
begin
  Open := false;
  OpenStateChangeTime := ParentLevel.AnimationTime;
  PlaySound(stDoorClose);
end;

procedure TDoomLevelDoor.RevertDoOpen;
begin
  Open := true;
  OpenStateChangeTime := { ParentLevel.AnimationTime -
    (OpenCloseTime - (ParentLevel.AnimationTime - OpenStateChangeTime)) }
    { simplified : }
    2 * ParentLevel.AnimationTime - OpenCloseTime - OpenStateChangeTime;
  PlaySound(stDoorOpen);
end;

function TDoomLevelDoor.SceneTranslation(const AnimationTime: Single):
  TVector3_Single;
begin
  if not Open then
  begin
    if AnimationTime - OpenStateChangeTime > OpenCloseTime then
      { Completely closed. }
      Result.Init_Zero else
      { During closing. }
      Result := OpenMove *
        (1 - (AnimationTime - OpenStateChangeTime) / OpenCloseTime);
  end else
  begin
    if AnimationTime - OpenStateChangeTime > OpenCloseTime then
      { Completely open. }
      Result := OpenMove else
      { During opening. }
      Result := OpenMove *
        ((AnimationTime - OpenStateChangeTime) / OpenCloseTime);
  end;
end;

function TDoomLevelDoor.CompletelyOpen: boolean;
begin
  Result := Open and
    (ParentLevel.AnimationTime - OpenStateChangeTime > OpenCloseTime);
end;

function TDoomLevelDoor.CompletelyClosed: boolean;
begin
  Result := (not Open) and
    (ParentLevel.AnimationTime - OpenStateChangeTime > OpenCloseTime);
end;

procedure TDoomLevelDoor.Render(const Frustum: TFrustum;
  const ATransparent: boolean);
begin
  if Exists and (ATransparent = Transparent) then
  begin
    { TODO: this should be implemented in TLevelMovingObject, in a manner
      that is able to use Scene.RenderFrustum always (by translating the
      frustum) }

    { The completely closed door is the most common case
      (since all doors close automatically, and initially all are closed...).
      Fortunately, it's also the case when we have constructed an octree,
      so we can efficiently render it by RenderFrustum call. }
    if CompletelyClosed then
      Scene.RenderFrustum(Frustum) else
      begin
        glPushMatrix;
          glTranslatev(SceneTranslation(ParentLevel.AnimationTime));
          Scene.Render(nil);
        glPopMatrix;
      end;
  end;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create;
var
  DoomDoorsPathPrefix: string;

  function MakeDoor(const FileName: string): TDoomLevelDoor;
  begin
    Result := TDoomLevelDoor.Create(Self, DoomDoorsPathPrefix + FileName);

    { Although I didn't know it initially, it turns out that all doors
      on Doom E1M1 level (maybe all doors totally ?) have the same
      values for parameters below. }
    Result.OpenCloseTime := 1.0;
    Result.OpenMove.Init(0, 0, 3.5);
    Result.StayOpenTime := 5.0;

    { All doors are initially closed. }
    Result.Open := false;

    { We set Result.OpenStateChangeTime to a past time, to be sure
      that we don't treat the door as "closing right now". }
    Result.OpenStateChangeTime := - 10 * Result.OpenCloseTime;

    Result.UsedSound := nil;
  end;

begin
  inherited;

  {}//PlayedMusicSound := stDoomE1M1Music;
  {TODO: use elevator}

  if Headlight <> nil then
  begin
    Headlight.AmbientColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.DiffuseColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
    Headlight.SpecularColor := Vector4Single(0.5, 0.5, 0.5, 1.0);
  end;

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

  HintOpenDoorShown := false;
end;

destructor TDoomE1M1Level.Destroy;
begin
  FreeAndNil(Elevator49);

  inherited;
end;

class function TDoomE1M1Level.SceneFileName: string;
begin
  Result := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_final.wrl';
end;

class function TDoomE1M1Level.LightSetFileName: string;
begin
  Result := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' + PathDelim + 'e1m1_lights.wrl';
end;

class function TDoomE1M1Level.Title: string;
begin
  Result := 'Doom E1M1';
end;

class function TDoomE1M1Level.Number: Integer;
begin
  Result := 90;
end;

procedure TDoomE1M1Level.Idle(const CompSpeed: Single);

  function SomethingWillBlockClosingDoor(const Door: TDoomLevelDoor;
    const NewAnimationTime: Single): boolean;
  var
    DoorBox: TBox3d;
    I: Integer;
  begin
    DoorBox := Box3dTranslate(Door.Scene.BoundingBox,
      Door.SceneTranslation(NewAnimationTime));

    Result := Boxes3dCollision(DoorBox, Player.BoundingBox);
    if Result then
      Exit;

    for I := 0 to Creatures.High do
    begin
      Result := Boxes3dCollision(DoorBox, Creatures[I].BoundingBox);
      if Result then
        Exit;
    end;
  end;

var
  I: Integer;
  Door: TDoomLevelDoor;
  NewAnimationTime: Single;
begin
  { First (before calling inherited) check for all the doors
    that are during closing: if the player or creatures will collide
    with them after inherited will change AnimationTime,
    then we must stop and open again (to avoid
    entering into collision with player/creature because of
    door move). }
  NewAnimationTime := AnimationTime + CompSpeed / 50;
  for I := 0 to Objects.High do
    if Objects[I] is TDoomLevelDoor then
    begin
      Door := TDoomLevelDoor(Objects[I]);

      if (not Door.Open) and
        (AnimationTime - Door.OpenStateChangeTime < Door.OpenCloseTime) and
        SomethingWillBlockClosingDoor(Door, NewAnimationTime) then
        Door.RevertDoOpen;
    end;

  inherited;

  for I := 0 to Objects.High do
    if Objects[I] is TDoomLevelDoor then
    begin
      Door := TDoomLevelDoor(Objects[I]);

      if Door.Open and
        (AnimationTime - Door.OpenStateChangeTime >
          Door.OpenCloseTime + Door.StayOpenTime) then
        Door.DoClose;

      { If the door open/close sound is longer than the Door.OpenCloseTime,
        stop this sound after the door is completely opened/closed. }
      if (AnimationTime - Door.OpenStateChangeTime > Door.OpenCloseTime) and
        (Door.UsedSound <> nil) then
        Door.UsedSound.DoUsingEnd;
    end;

  if (not HintOpenDoorShown) and
    Box3dPointInside(Player.Navigator.CameraPos, FHintOpenDoorBox) then
  begin
    HintOpenDoorShown := true;
    TimeMessage('Hint: open doors using the ' + InteractKeyDescription);
  end;
end;

procedure TDoomE1M1Level.SpecialObjectPicked(const Distance: Single;
  SpecialObjectIndex: Integer);
var
  Door: TDoomLevelDoor;
begin
  inherited;

  if Between(SpecialObjectIndex, 0, Objects.High) and
     (Objects[SpecialObjectIndex] is TDoomLevelDoor) then
  begin
    Door := TDoomLevelDoor(Objects[SpecialObjectIndex]);
    if Distance > 7 then
      TimeMessage('You see a door. You''re too far to open it from here') else
    { Only if the door is completely closed
      (and not during closing right now) we allow player to open it. }
    if not Door.CompletelyClosed then
      TimeMessage('You see a door. It''s already open') else
      Door.DoOpen;
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
  RemoveBoxNodeCheck(FHintOpenDoorBox, 'HintOpenDoorBox');
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