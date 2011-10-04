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

{ TLevel specialized descendants. }
unit GameLevelSpecific;

interface

uses VRMLGLScene, Boxes3D, VectorMath,
  GamePlayer, GameLevel, VRMLGLBackground, VRMLTriangle,
  GameSound, VRMLNodes, DOM, Base3D, VRMLGLAnimation,
  GameCreatures, Classes, CastleTimeUtils;

const
  CastleHallWerewolvesCount = 4;

type
  TCastleHallLevel = class(TLevel)
  private
    Symbol: T3DPrecalculatedAnimation;
    Button: T3DPrecalculatedAnimation;

    StairsBlocker: T3DScene;
    StairsBlockerMiddle: TVector3Single;

    FLevelExitBox: TBox3D;

    WerewolfAppearPosition: array [0..CastleHallWerewolvesCount - 1] of TVector3Single;
    WerewolfAppeared: boolean;
    WerewolfCreature: array [0..CastleHallWerewolvesCount - 1] of TWerewolfCreature;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure Picked(const Distance: Single;
      CollisionInfo: T3DCollision;
      var InteractionOccurred: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; override;
  end;

  TGateLevel = class(TLevel)
  private
    FGateExitBox: TBox3D;

    Teleport: T3DScene;
    FTeleport1Box, FTeleport2Box: TBox3D;

    Teleport1Rotate: Single;
    Teleport2Rotate: Single;

    Teleport1Destination: TVector3Single;
    Teleport2Destination: TVector3Single;

    SacrilegeAmbushStartingPosition: array [0..5] of TVector3Single;
    SwordAmbushStartingPosition: array [0..2] of TVector3Single;

    SacrilegeAmbushDone: boolean;
    SwordAmbushDone: boolean;

    FSacrilegeBox: TBox3D;

    CartLastSoundTime: Single;
    CartSoundPosition: TVector3Single;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;
    destructor Destroy; override;

    function CollisionIgnoreItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure Render3D(const Params: TRenderParams); override;

    procedure RenderShadowVolume; override;
  end;

  TTowerLevel = class(TLevel)
  private
    MovingElevator: TLevelLinearMovingObject;
    Elevator: T3DScene;
    ElevatorButton: T3DPrecalculatedAnimation;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;

    procedure Picked(const Distance: Single;
      CollisionInfo: T3DCollision;
      var InteractionOccurred: boolean); override;
  end;

  TCagesLevel = class(TLevel)
  private
    FSpidersAppearing: TVector3SingleList;
    NextSpidersAppearingTime: Single;

    HintOpenDoor: TLevelHintArea;

    FGateExit: T3DScene;

    FDoEndSequence: boolean;

    FEndSequence: T3DScene;
    procedure SetDoEndSequence(Value: boolean);
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;
    destructor Destroy; override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Render3D(const Params: TRenderParams); override;

    procedure RenderShadowVolume; override;

    { True means that GateExit will not be rendered (or collided)
      and EndSequence will be rendered. }
    property DoEndSequence: boolean
      read FDoEndSequence write SetDoEndSequence default false;

    procedure Picked(const Distance: Single;
      CollisionInfo: T3DCollision;
      var InteractionOccurred: boolean); override;

    function Background: TVRMLGLBackground; override;
  end;

  TDoomLevelDoor = class(TLevelLinearMovingObject)
  public
    StayOpenTime: Single;

    constructor Create(AOwner: TComponent); override;

    procedure BeforeTimeIncrease(const NewAnimationTime: TFloatTime); override;
    procedure Idle(const CompSpeed: Single); override;

    property MovePushesOthers default false;

    { No way to express this:
    property SoundGoBeginPosition default stDoorClose;
    property SoundGoEndPosition default stDoorOpen;
    }
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TX3DNode);
  private
    FakeWall: T3DScene;

    MovingElevator49: TLevelLinearMovingObject;
    Elevator49: T3DScene;
    Elevator49DownBox: TBox3D;

    MovingElevator9a9b: TLevelLinearMovingObject;
    Elevator9a9b: T3DScene;
    Elevator9a9bPickBox: TBox3D;

    ExitButton: T3DScene;
    ExitMessagePending: boolean;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;
    destructor Destroy; override;

    procedure Picked(const Distance: Single;
      CollisionInfo: T3DCollision;
      var InteractionOccurred: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  end;

  TGateBackgroundLevel = class(TLevel)
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;
  end;

  TFountainLevel = class(TLevel)
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AName: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      ARequiredCreatures: TStringList;
      AMenuBackground: boolean); override;
    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

function CastleLevelsPath: string;

implementation

uses CastleFilesUtils, SysUtils, CastleUtils,
  GL, GLU, CastleGLUtils, CastleStringUtils, CastleMessages, RenderingCameraUnit,
  GamePlay, GameNotifications, GameInputs,
  GameItems, GameThunder, GameWindow, GameVRMLProcessing,
  GameAnimationTricks, GameVideoOptions, VRMLScene, ProgressUnit,
  CastleXMLUtils;

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  CastleHallLevelPath: string;
begin
  inherited;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := LoadLevelAnimation(CastleHallLevelPath + 'symbol.kanim', true, false);
  Symbol.CastsShadow := false; { shadow would not be visible anyway }
  Items.Add(Symbol);

  Button := LoadLevelAnimation(CastleHallLevelPath + 'button.kanim', true, false);
  Button.CastsShadow := false; { strange ghost shadow on symbol would be visible }
  Items.Add(Button);

  StairsBlocker := LoadLevelScene(CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl',
    true { create octrees }, false);
  StairsBlocker.CastsShadow := false; { shadow would not be visible anyway }
  Items.Add(StairsBlocker);

  { get StairsBlocker.BoundingBox.Middle when it Exists.
    Later StairsBlocker will have Exists = false, so bbox will be empty,
    but we'll need StairsBlockerMiddle position. }
  StairsBlockerMiddle := StairsBlocker.BoundingBox.Middle;
end;

procedure TCastleHallLevel.ChangeLevelScene;

  function BoxDownPosition(const Box: TBox3D): TVector3Single;
  begin
    Result[0] := (Box.Data[0, 0] + Box.Data[1, 0]) / 2;
    Result[1] := (Box.Data[0, 1] + Box.Data[1, 1]) / 2;
    Result[2] := Box.Data[0, 2];
  end;

var
  TempBox: TBox3D;
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

procedure TCastleHallLevel.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
const
  WerewolfFirstLight = 1;

  procedure WerewolfAppear;
  var
    I: Integer;
    LightNode: TAbstractPositionalLightNode;
    Headlight: TLightInstance;
    ShadowLight: PLightInstance;
  begin
    Assert(not WerewolfAppeared);

    for I := 0 to CastleHallWerewolvesCount - 1 do
    begin
      WerewolfCreature[I] := Werewolf.CreateDefaultCreature(
        WerewolfAppearPosition[I],
        VectorSubtract(Player.Camera.Position, WerewolfAppearPosition[I]),
        AnimationTime, BaseLights, Werewolf.DefaultMaxLife) as TWerewolfCreature;
      Creatures.Add(WerewolfCreature[I]);
    end;

    WerewolfAppeared := true;

    WerewolfCreature[0].Howl(true);

    { change the lights }
    if HeadlightInstance(Headlight) then
    begin
      Headlight.Node.FdAmbientIntensity.Send(0.8);
      Headlight.Node.FdColor.Send(Vector3Single(1, 0, 0));
      Headlight.Node.FdIntensity.Send(0.2);
    end;

    for I := 0 to CastleHallWerewolvesCount - 1 do
    begin
      LightNode := MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
        TAbstractPositionalLightNode;
      LightNode.FdColor.Send(Vector3Single(1, 0, 0));
      LightNode.FdAttenuation.Send(Vector3Single(1, 0.1, 0));
      LightNode.FdKambiShadows.Send(true);
    end;

    ShadowLight := MainScene.GlobalLights.FindName('FakeShadowPosition');
    Check(ShadowLight <> nil, 'FakeShadowPosition light not found on castle_hall level');
    ShadowLight^.Node.FdKambiShadows.Send(true);
    ShadowLight^.Node.FdKambiShadowsMain.Send(true);
  end;

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
      LightNode := MainScene.GlobalLights.Items[WerewolfFirstLight].Node as
        TAbstractPositionalLightNode;
      LightNode.FdLocation.Value := StairsBlockerMiddle;
      LightNode.FdOn.Value := true;

      for I := 1 to CastleHallWerewolvesCount - 1 do
      begin
        LightNode := MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
          TAbstractPositionalLightNode;
        LightNode.FdOn.Value := false;
      end;
    end else
    begin
      { turn light for each alive werewolf }
      for I := 0 to CastleHallWerewolvesCount - 1 do
      begin
        LightNode := MainScene.GlobalLights.Items[I + WerewolfFirstLight].Node as
          TAbstractPositionalLightNode;
        LightNode.FdOn.Send(not WerewolfCreature[I].Dead);
        LightNode.FdLocation.Send(WerewolfCreature[I].MiddlePosition);
      end;
    end;
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

  if Player = nil then Exit;

  if FLevelExitBox.PointInside(Player.Camera.Position) then
  begin
    LevelFinished('cages');
  end;

  if Button.TimePlaying and
    (Button.Time > Button.TimeDuration) then
  begin
    if not Symbol.TimePlaying then
    begin
      Symbol.TimePlaying := true;
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
  CollisionInfo: T3DCollision;
  var InteractionOccurred: boolean);
begin
  inherited;

  if CollisionInfo.Hierarchy.IndexOf(StairsBlocker) <> -1 then
  begin
    InteractionOccurred := true;
    NotificationInteractFailed('You are not able to open it');
  end else
  if CollisionInfo.Hierarchy.IndexOf(Button) <> -1 then
  begin
    InteractionOccurred := true;
    if Distance < 10.0 then
    begin
      if Button.TimePlaying then
        NotificationInteractFailed('Button is already pressed') else
      begin
        Button.TimePlaying := true;
        Notifications.Show('You press the button');
      end;
    end else
      NotificationInteractFailed('You see a button. You cannot reach it from here');
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
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  Cart: T3DPrecalculatedAnimation;
  GateLevelPath: string;
begin
  inherited;

  GateLevelPath := CastleLevelsPath + 'gate' + PathDelim;

  Teleport := LoadLevelScene(GateLevelPath + 'teleport.wrl', false, false);

  Cart := LoadLevelAnimation(GateLevelPath + 'cart.kanim', true, true);
  Cart.CollisionUseLastScene := true;
  Items.Add(Cart);
  Cart.TimePlaying := true;

  CartSoundPosition := Cart.FirstScene.BoundingBox.Middle;

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
end;

destructor TGateLevel.Destroy;
begin
  FreeAndNil(Teleport);
  inherited;
end;

procedure TGateLevel.ChangeLevelScene;

  function AmbushStartingPos(const Box: TBox3D): TVector3Single;
  begin
    Result[0] := (Box.Data[0, 0] + Box.Data[1, 0]) / 2;
    Result[1] := (Box.Data[0, 1] + Box.Data[1, 1]) / 2;
    Result[2] := Box.Data[0, 2];
  end;

var
  TempBox: TBox3D;
  I: Integer;
begin
  inherited;

  RemoveBoxNodeCheck(FGateExitBox, 'GateExitBox');

  RemoveBoxNodeCheck(FTeleport1Box, 'Teleport1Box');
  RemoveBoxNodeCheck(FTeleport2Box, 'Teleport2Box');

  RemoveBoxNodeCheck(FSacrilegeBox, 'SacrilegeBox');

  Teleport1Destination := FTeleport2Box.Middle;
  Teleport1Destination[0] += 2;
  Teleport1Destination[1] += 2;

  Teleport2Destination := FTeleport1Box.Middle;
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

procedure TGateLevel.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);

  procedure RejectGateExitBox;
  var
    NewPosition: TVector3Single;
  begin
    NewPosition := Player.Camera.Position;
    { Although I do him knockback, I also change the position
      to make sure that he is thrown outside of FGateExitBox. }
    NewPosition[1] := FGateExitBox.Data[0, 1] - 0.1;
    Player.Camera.Position := NewPosition;

    Player.Knockback(0, 2, Vector3Single(0, -1, 0));
  end;

  procedure TeleportWork(const TeleportBox: TBox3D;
    const Destination: TVector3Single);
  begin
    if TeleportBox.PointInside(Player.Camera.Position) then
    begin
      Player.Camera.Position := Destination;
      Player.Camera.CancelFallingDown;

      MainScene.ViewChangedSuddenly;

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
      CreatureDirection := VectorSubtract(Player.Camera.Position,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, BaseLights, Ghost.DefaultMaxLife);
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
      CreatureDirection := VectorSubtract(Player.Camera.Position,
        CreaturePosition);
      Creature := Ghost.CreateDefaultCreature(CreaturePosition,
        CreatureDirection, AnimationTime, BaseLights, Ghost.DefaultMaxLife);
      Creatures.Add(Creature);
    end;
  end;

const
  { In seconds. }
  CartSoundRepeatTime = 10.0;
begin
  inherited;

  if Player = nil then Exit;

  if FGateExitBox.PointInside(Player.Camera.Position) then
  begin
    if Player.Items.FindKind(KeyItemKind) = -1 then
    begin
      Notifications.Show('You need a key to open this door');
      RejectGateExitBox;
    end else
    if Player.Items.FindKind(Sword) = -1 then
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
    Teleport1Rotate += 0.2 * CompSpeed * 50;
    Teleport2Rotate += 0.2 * CompSpeed * 50;
    TeleportWork(FTeleport1Box, Teleport1Destination);
    TeleportWork(FTeleport2Box, Teleport2Destination);

    if (not SacrilegeAmbushDone) and
      FSacrilegeBox.PointInside(Player.Camera.Position) then
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

  if AnimationTime - CartLastSoundTime > CartSoundRepeatTime then
  begin
    CartLastSoundTime := AnimationTime;
    SoundEngine.Sound3d(stCreak, CartSoundPosition);
  end;
end;

function TGateLevel.CollisionIgnoreItem(
  const Sender: TObject; const Triangle: P3DTriangle): boolean;
begin
  Result :=
    (inherited CollisionIgnoreItem(Sender, Triangle)) or
    (PTriangle(Triangle)^.State.LastNodes.Material.NodeName = 'MatWater');
end;

procedure TGateLevel.Render3D(const Params: TRenderParams);

{ TODO: remake teleport as simply additional object on level, not collidable.
  This will remove the need for below.
  Required: T3DTransform (see draft tutorial for planned name),
  like T3DTranslated. Maybe special T3DTranslateRotate ?

  This will also fix: for now, we ignore Params.Transparent below,
  and render teleports transparent/opaque invalid.
}

  procedure RenderTeleport(
    const TeleportRotation: Single;
    const TeleportBox: TBox3D;
    const Transparent: boolean);
  begin
    if RenderingCamera.Frustum.Box3DCollisionPossibleSimple(TeleportBox) then
    begin
      glPushMatrix;
        glTranslatev(TeleportBox.Middle);
        glRotatef(TeleportRotation, 1, 1, 0);
        Teleport.Render(nil, Params);
      glPopMatrix;
    end;
  end;

begin
  RenderTeleport(Teleport1Rotate, FTeleport1Box, Params.Transparent);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, Params.Transparent);

  inherited;

  RenderTeleport(Teleport1Rotate, FTeleport1Box, Params.Transparent);
  RenderTeleport(Teleport2Rotate, FTeleport2Box, Params.Transparent);
end;

procedure TGateLevel.RenderShadowVolume;
begin
  { TODO: render teleport shadow quads }
  inherited;
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  TowerLevelPath: string;
begin
  inherited;

  TowerLevelPath := CastleLevelsPath + 'tower' + PathDelim;

  Elevator := LoadLevelScene(TowerLevelPath + 'elevator.wrl', true, false);

  ElevatorButton := LoadLevelAnimation(TowerLevelPath + 'elevator_button.kanim', true, false);

  MovingElevator := TLevelLinearMovingObject.Create(Self);
  MovingElevator.Add(Elevator);
  MovingElevator.Add(ElevatorButton);
  MovingElevator.MoveTime := 10.0;
  MovingElevator.TranslationEnd := Vector3Single(0, 0, 122);
  MovingElevator.SoundGoEndPosition := stElevator;
  MovingElevator.SoundGoEndPositionLooping := true;
  MovingElevator.SoundGoBeginPosition := stElevator;
  MovingElevator.SoundGoBeginPositionLooping := true;
  MovingElevator.SoundTracksCurrentPosition := true;
  { no shadow, because looks bad: tower level has uninteresting light
    and elevator triggers artifacts because of BorderEdges. }
  MovingElevator.CastsShadow := false;
  Items.Add(MovingElevator);
end;

procedure TTowerLevel.Picked(const Distance: Single;
  CollisionInfo: T3DCollision;
  var InteractionOccurred: boolean);
begin
  inherited;

  if CollisionInfo.Hierarchy.IndexOf(ElevatorButton) <> -1 then
  begin
    InteractionOccurred := true;
    if Distance > 10 then
      NotificationInteractFailed(
        'You see a button. You''re too far to reach it from here') else
    begin
      { play from the beginning }
      ElevatorButton.ResetTimeAtLoad;
      ElevatorButton.TimePlaying := true;
      MovingElevator.GoOtherPosition;
    end;
  end;
end;

{ TCagesLevel ---------------------------------------------------------------- }

constructor TCagesLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  BossIndex: Integer;
begin
  inherited;

  ThunderEffect := TThunderEffect.Create;

  FSpidersAppearing := TVector3SingleList.Create;
  NextSpidersAppearingTime := 0;

  { TODO: this is not nice; I should add TLevelObject.Name for such
    purposes, and use here Items.FindName('hint_button_box'). }
  HintOpenDoor := Items.List[1] as TLevelHintArea;

  FEndSequence := LoadLevelScene(
    CastleLevelsPath + 'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    true { create octrees },
    true { true: load background of EndSequence; we will use it });
  FEndSequence.Exists := false;
  { Even when FEndSequence will exist, we will not check for collisions
    with it --- no reason to waste time, no collisions will be possible
    as player's move along the EndSequence will be programmed. }
  FEndSequence.Collides := false;
  FEndSequence.CastsShadow := false; { shadow is not visible anyway }
  Items.Add(FEndSequence);

  FGateExit := LoadLevelScene(
    CastleLevelsPath + 'cages' + PathDelim + 'cages_gate_exit.wrl',
    true { create octrees }, false);
  FGateExit.CastsShadow := false; { shadow is not visible anyway }
  Items.Add(FGateExit);

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

procedure TCagesLevel.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
const
  { Some SpiderRadius is used to not put spider inside the wall. }
  SpiderRadius = 2;
  MinSpiderX = -11.0  + SpiderRadius;
  MaxSpiderX = 69.0   - SpiderRadius;
  MinSpiderY = -123.0 + SpiderRadius;
  MaxSpiderY = 162.0  - SpiderRadius;

  procedure AppearSpider(const Position: TVector3Single);
  begin
    FSpidersAppearing.Add(Position);
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
    Result[0] := Player.Camera.Position[0] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[0] := Clamped(Result[0], MinSpiderX, MaxSpiderX);
    Result[1] := Player.Camera.Position[1] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[1] := Clamped(Result[1], MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

const
  SpidersFallingSpeed = 0.5;
  CreaturesCountToAddSpiders = 20;
var
  IsAbove: boolean;
  AboveHeight: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3Single;
  SpiderMoveDistance: Single;
  AboveGround: PTriangle;
var
  TorchLight: PLightInstance;
begin
  inherited;

  if Player = nil then Exit;

  if not GameWin then
  begin
    { Torch light modify, to make an illusion of unstable light }
    TorchLight := MainScene.GlobalLights.FindName('MainHallTorchLight');
    Check(TorchLight <> nil, 'Torch light not found on cages level');
    TorchLight^.Node.FdIntensity.Send(Clamped(
        TorchLight^.Node.FdIntensity.Value +
          MapRange(Random, 0, 1, -0.1, 0.1) * CompSpeed  * 50,
        0.5, 1));

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
    IsAbove := false;
    AboveHeight := MaxSingle;
    AboveGround := nil;
    while I < FSpidersAppearing.Count do
    begin
      GetHeightAbove(FSpidersAppearing.Items[I], IsAbove,
        AboveHeight, AboveGround);
      if AboveHeight < Spider.CameraRadius * 2 then
      begin
        SpiderPosition := FSpidersAppearing.Items[I];
        SpiderDirection :=
          VectorSubtract(Player.Camera.Position, SpiderPosition);
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, Level.GravityUp);
        SpiderCreature := Spider.CreateDefaultCreature(
          SpiderPosition, SpiderDirection, AnimationTime, BaseLights, Spider.DefaultMaxLife);
        Creatures.Add(SpiderCreature);
        SpiderCreature.Sound3d(stSpiderAppears, 1.0);
        FSpidersAppearing.Delete(I);
      end else
      begin
        { calculate SpiderMoveDistance }
        SpiderMoveDistance := SpidersFallingSpeed * CompSpeed * 50;
        MinTo1st(SpiderMoveDistance, AboveHeight - Spider.CameraRadius);
        FSpidersAppearing.L[I][2] -= SpiderMoveDistance;
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

procedure TCagesLevel.Render3D(const Params: TRenderParams);
var
  I: Integer;
begin
  if not Params.Transparent then
  begin
    { Render spiders before rendering inherited,
      because spiders are not transparent. }
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Black3Single);
      glBegin(GL_LINES);
        for I := 0 to FSpidersAppearing.Count - 1 do
        begin
          glVertex3f(FSpidersAppearing.Items[I][0],
                     FSpidersAppearing.Items[I][1], SpiderZ);
          glVertexv(FSpidersAppearing.Items[I]);
        end;
      glEnd;
    glPopAttrib;
  end;

  for I := 0 to FSpidersAppearing.Count - 1 do
  begin
    glPushMatrix;
      glTranslatev(FSpidersAppearing.Items[I]);
      Spider.StandAnimation.Scenes[0].Render(nil, Params);
    glPopMatrix;
  end;

  inherited;
end;

procedure TCagesLevel.RenderShadowVolume;
begin
  { TODO: render spiders shadow quads }
  inherited;
end;

procedure TCagesLevel.Picked(const Distance: Single;
  CollisionInfo: T3DCollision;
  var InteractionOccurred: boolean);
begin
  inherited;

  if Player = nil then Exit;

  if CollisionInfo.Hierarchy.IndexOf(FGateExit) <> -1 then
  begin
    InteractionOccurred := true;
    if Distance > 10 then
      NotificationInteractFailed(
        'You see a door. You''re too far to open it from here') else
    begin
      if Player.Items.FindKind(RedKeyItemKind) <> -1 then
      begin
        if (BossCreature <> nil) and (not BossCreature.Dead) then
        begin
          Player.Knockback(2 + Random(5), 2, Vector3Single(0, -1, 0));
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
end;

function TCagesLevel.Background: TVRMLGLBackground;
begin
  if DoEndSequence then
    Result := FEndSequence.Background else
    Result := inherited;
end;

{ TDoomLevelDoor ------------------------------------------------------------- }

constructor TDoomLevelDoor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MovePushesOthers := false;
  SoundGoEndPosition := stDoorOpen;
  SoundGoBeginPosition := stDoorClose;
  CastsShadow := false; { looks bad }
end;

procedure TDoomLevelDoor.BeforeTimeIncrease(const NewAnimationTime: TFloatTime);

  function SomethingWillBlockClosingDoor: boolean;
  var
    DoorBox: TBox3D;
    I: Integer;
  begin
    DoorBox := (inherited BoundingBox).Translate(
      GetTranslationFromTime(NewAnimationTime));

    Result := (Player <> nil) and DoorBox.Collision(Player.BoundingBox);
    if Result then
      Exit;

    for I := 0 to ParentLevel.Creatures.Count - 1 do
    begin
      Result := DoorBox.Collision(ParentLevel.Creatures[I].BoundingBox);
      if Result then
        Exit;
    end;

    for I := 0 to ParentLevel.ItemsOnLevel.Count - 1 do
    begin
      Result := DoorBox.Collision(ParentLevel.ItemsOnLevel[I].BoundingBox);
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
    (AnimationTime - EndPositionStateChangeTime < MoveTime) and
    SomethingWillBlockClosingDoor then
    RevertGoEndPosition;
end;

procedure TDoomLevelDoor.Idle(const CompSpeed: Single);
begin
  inherited;

  if EndPosition and
    (AnimationTime - EndPositionStateChangeTime >
      MoveTime + StayOpenTime) then
    GoBeginPosition;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  DoomDoorsPathPrefix: string;

  function MakeDoor(const FileName: string): TDoomLevelDoor;
  begin
    Result := TDoomLevelDoor.Create(Self);
    Result.Add(LoadLevelScene(DoomDoorsPathPrefix + FileName,
      true { create octrees }, false));

    { Although I didn't know it initially, it turns out that all doors
      on Doom E1M1 level (maybe all doors totally ?) have the same
      values for parameters below. }
    Result.MoveTime := 1.0;
    Result.TranslationEnd := Vector3Single(0, 0, 3.5);
    Result.StayOpenTime := 5.0;
  end;

begin
  inherited;

  DoomDoorsPathPrefix := CastleLevelsPath + 'doom' + PathDelim + 'e1m1' +
    PathDelim;

  Items.Add(MakeDoor('door2_3_closed.wrl'));
  Items.Add(MakeDoor('door4_5_closed.wrl'));
  Items.Add(MakeDoor('door4_7_closed.wrl'));
  Items.Add(MakeDoor('door5_6_closed.wrl'));

  FakeWall := LoadLevelScene( DoomDoorsPathPrefix + 'fake_wall_final.wrl',
    false { no need for octrees, does never collide }, false);
  FakeWall.Collides := false;
  FakeWall.CastsShadow := false;
  Items.Add(FakeWall);

  Elevator49 := LoadLevelScene(DoomDoorsPathPrefix + 'elevator4_9_final.wrl',
    true { create octrees }, false);

  MovingElevator49 := TLevelLinearMovingObject.Create(Self);
  MovingElevator49.Add(Elevator49);
  MovingElevator49.MoveTime := 3.0;
  MovingElevator49.TranslationEnd := Vector3Single(0, 0, -6.7);
  MovingElevator49.SoundGoEndPosition := stElevator;
  MovingElevator49.SoundGoEndPositionLooping := true;
  MovingElevator49.SoundGoBeginPosition := stElevator;
  MovingElevator49.SoundGoBeginPositionLooping := true;
  MovingElevator49.SoundTracksCurrentPosition := true;
  MovingElevator49.CastsShadow := false;
  Items.Add(MovingElevator49);

  Elevator9a9b := LoadLevelScene(DoomDoorsPathPrefix + 'elevator_9a_9b_final.wrl',
    true { create octrees }, false);

  MovingElevator9a9b := TLevelLinearMovingObject.Create(Self);
  MovingElevator9a9b.Add(Elevator9a9b);
  MovingElevator9a9b.MoveTime := 3.0;
  MovingElevator9a9b.TranslationEnd := Vector3Single(0, 0, -7.5);
  MovingElevator9a9b.SoundGoEndPosition := stElevator;
  MovingElevator9a9b.SoundGoEndPositionLooping := true;
  MovingElevator9a9b.SoundGoBeginPosition := stElevator;
  MovingElevator9a9b.SoundGoBeginPositionLooping := true;
  MovingElevator9a9b.SoundTracksCurrentPosition := true;
  MovingElevator9a9b.CastsShadow := false;
  Items.Add(MovingElevator9a9b);

  ExitButton := LoadLevelScene(DoomDoorsPathPrefix + 'exit_button_final.wrl',
    true { create octrees }, false);
  ExitButton.CastsShadow := false;
  Items.Add(ExitButton);
end;

destructor TDoomE1M1Level.Destroy;
begin
  inherited;
end;

procedure TDoomE1M1Level.Picked(const Distance: Single;
  CollisionInfo: T3DCollision;
  var InteractionOccurred: boolean);
var
  Door: TDoomLevelDoor;
begin
  inherited;

  if Player = nil then Exit;

  if (CollisionInfo.Hierarchy.Count > 1) and
    (CollisionInfo.Hierarchy[1] is TDoomLevelDoor) then
  begin
    Door := TDoomLevelDoor(CollisionInfo.Hierarchy[1]);
    InteractionOccurred := true;
    if Distance > 7 then
      NotificationInteractFailed('You see a door. You''re too far to open it from here') else
    { Only if the door is completely closed
      (and not during closing right now) we allow player to open it. }
    if not Door.CompletelyBeginPosition then
      NotificationInteractFailed('You see a door. It''s already open') else
      Door.GoEndPosition;
  end else
  if (CollisionInfo.Hierarchy.IndexOf(Elevator9a9b) <> -1) and
     MovingElevator9a9b.CompletelyBeginPosition and
     Elevator9a9bPickBox.PointInside(Player.Camera.Position) then
  begin
    InteractionOccurred := true;
    if Distance > 10 then
      NotificationInteractFailed(
        'You''re too far to reach it from here') else
      MovingElevator9a9b.GoEndPosition;
  end else
  if CollisionInfo.Hierarchy.IndexOf(ExitButton) <> -1 then
  begin
    InteractionOccurred := true;
    if Distance > 5 then
      NotificationInteractFailed(
        'You''re too far to reach it from here') else
      begin
        SoundEngine.Sound(stDoomExitButton);
        Player.Life := 0;
        ExitMessagePending := true;
      end;
  end;
end;

procedure TDoomE1M1Level.RenameCreatures(Node: TX3DNode);
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

  MainScene.RootNode.EnumerateNodes(@RenameCreatures, true);
  RemoveBoxNodeCheck(Elevator49DownBox, 'Elevator49DownBox');
  RemoveBoxNodeCheck(Elevator9a9bPickBox, 'Elev9a9bPickBox');
end;

procedure TDoomE1M1Level.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  NewPlayer.PickItem(TItem.Create(Bow, 1));
  NewPlayer.PickItem(TItem.Create(Quiver, 10));
end;

procedure TDoomE1M1Level.Idle(const CompSpeed: Single;
  const HandleMouseAndKeys: boolean;
  var LetOthersHandleMouseAndKeys: boolean);
begin
  inherited;

  if Player = nil then Exit;

  if MovingElevator49.CompletelyBeginPosition and
     Elevator49DownBox.PointInside(Player.Camera.Position) then
  begin
    MovingElevator49.GoEndPosition;
  end;

  if MovingElevator9a9b.CompletelyEndPosition and
     (AnimationTime - MovingElevator9a9b.EndPositionStateChangeTime >
       MovingElevator9a9b.MoveTime +
       { This is the time for staying in lowered position. }
       2.0) then
    MovingElevator9a9b.GoBeginPosition;

  if ExitMessagePending and (not Player.Camera.FallingOnTheGround) then
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
      'you''re most welcome to contribute!', taLeft);
    ExitMessagePending := false;
  end;
end;

{ TGateBackgroundLevel ------------------------------------------------------- }

constructor TGateBackgroundLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  Water: T3DPrecalculatedAnimation;
begin
  inherited;

  Water := LoadLevelAnimation(CastleLevelsPath + 'gate_background' +
    PathDelim + 'water.kanim', false, false);
  Water.CastsShadow := false; { water shadow would look awkward }
  { No octrees created for water (because in normal usage, player will not
    walk on this level). For safety, Collides set to @false, in case
    user enters this level by debug menu. }
  Water.Collides := false;
  Items.Add(Water);

  Water.TimePlaying := true;
end;

{ TFountainLevel ------------------------------------------------------------- }

constructor TFountainLevel.Create(
  const AName: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  ARequiredCreatures: TStringList;
  AMenuBackground: boolean);
var
  Fountain: TBlendedLoopingAnimation;
  LoadWaterAnimation: boolean;
begin
  inherited;

  if DOMGetBooleanAttribute(DOMElement, 'load_water_animation', LoadWaterAnimation)
    and LoadWaterAnimation then
  begin
    { load Fountain animation, following the similar code as LoadLevelAnimation }
    Fountain := TBlendedLoopingAnimationShader.CreateCustomCache(Self, GLContextCache);
    Fountain.LoadFromFile(CastleLevelsPath + 'fountain' +
      PathDelim + 'water_stream' + PathDelim + 'fountain.kanim', false, true);
    AnimationAttributesSet(Fountain.Attributes, btIncrease);
    Progress.Init(Fountain.PrepareResourcesSteps, 'Loading water');
    try
      Fountain.PrepareResources([prRender, prBoundingBox], true, BaseLights);
    finally Progress.Fini end;
    Fountain.FreeResources([frTextureDataInNodes]);
    Fountain.CastsShadow := false; { not manifold }
    Fountain.Collides := false;

    Fountain.Attributes.BlendingDestinationFactor := GL_ONE_MINUS_SRC_ALPHA;

    Fountain.TimePlayingSpeed := 1.5;
    Fountain.TimePlaying := true;

    Items.Add(Fountain);
  end;
end;

procedure TFountainLevel.ChangeLevelScene;
begin
  inherited;
  LevelFountainProcess(MainScene.RootNode);
end;

procedure TFountainLevel.PrepareNewPlayer(NewPlayer: TPlayer);
begin
  inherited;

  { Give player 1 sword. Otherwise player would start the level
    without any weapon, and there's no weapon to be found on
    the level... }
  NewPlayer.PickItem(TItem.Create(Sword, 1));
end;

end.