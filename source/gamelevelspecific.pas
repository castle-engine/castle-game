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

{ TLevel specialized descendants. }
unit GameLevelSpecific;

interface

uses CastleScene, Boxes3D, VectorMath,
  GamePlayer, GameLevel, Background, Triangle, GameObjectKinds,
  GameSound, X3DNodes, DOM, Base3D, PrecalculatedAnimation,
  GameCreatures, Classes, CastleTimeUtils, CastleColors, Frustum;

const
  CastleHallWerewolvesCount = 4;

type
  TCastleHallLevel = class(TLevel)
  private
    Symbol: TCastlePrecalculatedAnimation;
    Button: TCastlePrecalculatedAnimation;

    StairsBlocker: TCastleScene;
    StairsBlockerMiddle: TVector3Single;

    FLevelExitBox: TBox3D;

    WerewolfAppearPosition: array [0..CastleHallWerewolvesCount - 1] of TVector3Single;
    WerewolfAppeared: boolean;
    WerewolfCreature: array [0..CastleHallWerewolvesCount - 1] of TWerewolfCreature;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    function BossCreatureIndicator(out Life, MaxLife: Single): boolean; override;
  end;

  TGateLevel = class(TLevel)
  private
    FGateExitBox: TBox3D;

    Teleport: TCastleScene;
    Teleport1, Teleport2: T3DTransform;
    Teleport1Box, Teleport2Box: TBox3D;
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
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;

    function CollisionIgnoreItem(
      const Sender: TObject;
      const Triangle: P3DTriangle): boolean; override;
    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  end;

  TTowerLevel = class(TLevel)
  private
    MovingElevator: T3DLinearMoving;
    Elevator: TCastleScene;
    ElevatorButton: TCastlePrecalculatedAnimation;
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;
  end;

  TSpiderAppearing = class(T3DTransform)
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
  end;

  TGameWinAnimation = (
    gwaNone,
    gwaAnimateTo1,
    gwaAnimateTo2,
    gwaFinished);

  TCagesLevel = class(TLevel)
  private
    { List of TSpiderAppearing instances }
    SpidersAppearing: T3DList;
    NextSpidersAppearingTime: Single;

    HintOpenDoor: TLevelHintArea;

    FGateExit: TCastleScene;

    FGameWinAnimation: TGameWinAnimation;

    FEndSequence: TCastleScene;
    procedure SetGameWinAnimation(Value: TGameWinAnimation);
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    { Stage of game win animation. Known by this level, to display appropriate
      geometry and background. }
    property GameWinAnimation: TGameWinAnimation
      read FGameWinAnimation write SetGameWinAnimation default gwaNone;

    function Background: TBackground; override;
  end;

  TDoomLevelDoor = class(T3DLinearMoving)
  public
    StayOpenTime: Single;

    constructor Create(AOwner: TComponent); override;

    procedure BeforeTimeIncrease(const NewAnimationTime: TFloatTime); override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    property Pushes default false;

    { No way to express this:
    property SoundGoBeginPosition default stDoorClose;
    property SoundGoEndPosition default stDoorOpen;
    }

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

  TDoomE1M1Level = class(TLevel)
  private
    procedure RenameCreatures(Node: TX3DNode);
  private
    FakeWall: TCastleScene;

    MovingElevator49: T3DLinearMoving;
    Elevator49: TCastleScene;
    Elevator49DownBox: TBox3D;

    MovingElevator9a9b: T3DLinearMoving;
    Elevator9a9b: TCastleScene;
    Elevator9a9bPickBox: TBox3D;

    ExitButton: TCastleScene;
    ExitMessagePending: boolean;
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;

    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;

    procedure Idle(const CompSpeed: Single;
      const HandleMouseAndKeys: boolean;
      var LetOthersHandleMouseAndKeys: boolean); override;
  end;

  TGateBackgroundLevel = class(TLevel)
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;
  end;

  TFountainLevel = class(TLevel)
  protected
    procedure ChangeLevelScene; override;
  public
    constructor Create(
      const AId: string;
      const ASceneFileName: string;
      const ATitle: string; const ATitleHint: string; const ANumber: Integer;
      DOMElement: TDOMElement;
      AResources: T3DResourceList;
      AMenuBackground: boolean); override;
    procedure PrepareNewPlayer(NewPlayer: TPlayer); override;
  end;

function CastleLevelsPath: string;

implementation

uses CastleFilesUtils, SysUtils, CastleUtils,
  GL, GLU, CastleGLUtils, CastleStringUtils, CastleMessages, RenderingCameraUnit,
  GamePlay, GameNotifications, GameInputs,
  GameItems, GameThunder, GameWindow, GameX3DProcessing,
  GameAnimationTricks, GameVideoOptions, CastleSceneCore, ProgressUnit,
  CastleXMLUtils;

function CastleLevelsPath: string;
begin
  Result := ProgramDataPath + 'data' + PathDelim + 'levels' + PathDelim;
end;

procedure NotificationInteractFailed(const S: string);
begin
  Notifications.Show(S);
  SoundEngine.Sound(stPlayerInteractFailed);
end;

{ TStairsBlocker ------------------------------------------------------------- }

type
  TStairsBlocker = class(TCastleScene)
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TStairsBlocker.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

  NotificationInteractFailed('You are not able to open it');
end;

{ TCastleHallButton ---------------------------------------------------------- }

type
  TCastleHallButton = class(TCastlePrecalculatedAnimation)
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TCastleHallButton.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

  if Distance < 10.0 then
  begin
    if TimePlaying then
      NotificationInteractFailed('Button is already pressed') else
    begin
      TimePlaying := true;
      Notifications.Show('You press the button');
    end;
  end else
    NotificationInteractFailed('You see a button. You cannot reach it from here');
end;

{ TCastleHallLevel ----------------------------------------------------------- }

constructor TCastleHallLevel.Create(
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
  AMenuBackground: boolean);
var
  CastleHallLevelPath: string;
begin
  inherited;

  CastleHallLevelPath := CastleLevelsPath + 'castle_hall' + PathDelim;

  Symbol := LoadLevelAnimation(CastleHallLevelPath + 'symbol.kanim', true, false);
  Symbol.CastShadowVolumes := false; { shadow would not be visible anyway }
  Items.Add(Symbol);

  Button := LoadLevelAnimation(CastleHallLevelPath + 'button.kanim', true, false,
    TCastleHallButton);
  Button.CastShadowVolumes := false; { strange ghost shadow on symbol would be visible }
  Items.Add(Button);

  StairsBlocker := LoadLevelScene(CastleHallLevelPath + 'castle_hall_stairs_blocker.wrl',
    true { create octrees }, false, TStairsBlocker);
  StairsBlocker.CastShadowVolumes := false; { shadow would not be visible anyway }
  Items.Add(StairsBlocker);

  { get StairsBlocker.BoundingBox.Middle when it GetExists.
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
      WerewolfCreature[I] := Werewolf.CreateCreature(Items,
        WerewolfAppearPosition[I],
        VectorSubtract(Player.Position, WerewolfAppearPosition[I]))
        as TWerewolfCreature;

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
        LightNode.FdLocation.Send(WerewolfCreature[I].Middle);
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

  if (Player = nil) or Paused then Exit;

  if FLevelExitBox.PointInside(Player.Position) then
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
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
  AMenuBackground: boolean);
var
  Cart: TCastlePrecalculatedAnimation;
  GateLevelPath: string;
begin
  inherited;

  GateLevelPath := CastleLevelsPath + 'gate' + PathDelim;

  Teleport := LoadLevelScene(GateLevelPath + 'teleport.wrl', false, false);
  Teleport.Collides := false;
  Teleport.CastShadowVolumes := false; { not manifold }

  Teleport1 := T3DTransform.Create(Self);
  { set rotation axis. Rotation angle will be increased in each Idle }
  Teleport1.Rotation :=  Vector4Single(1, 1, 0, 0);
  Teleport1.Translation := Teleport1Box.Middle;
  Teleport1.Add(Teleport);
  Items.Add(Teleport1);

  Teleport2 := T3DTransform.Create(Self);
  Teleport2.Rotation :=  Vector4Single(1, 1, 0, 0);
  Teleport2.Translation := Teleport2Box.Middle;
  Teleport2.Add(Teleport);
  Items.Add(Teleport2);

  Cart := LoadLevelAnimation(GateLevelPath + 'cart.kanim', true, true);
  Cart.CollisionUseLastScene := true;
  Items.Add(Cart);
  Cart.TimePlaying := true;

  CartSoundPosition := Cart.FirstScene.BoundingBox.Middle;

  SacrilegeAmbushDone := false;
  SwordAmbushDone := false;
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

  RemoveBoxNodeCheck(Teleport1Box, 'Teleport1Box');
  RemoveBoxNodeCheck(Teleport2Box, 'Teleport2Box');

  RemoveBoxNodeCheck(FSacrilegeBox, 'SacrilegeBox');

  Teleport1Destination := Teleport2Box.Middle;
  Teleport1Destination[0] += 2;
  Teleport1Destination[1] += 2;

  Teleport2Destination := Teleport1Box.Middle;
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
    NewPosition := Player.Position;
    { Although I do him knockback, I also change the position
      to make sure that he is thrown outside of FGateExitBox. }
    NewPosition[1] := FGateExitBox.Data[0, 1] - 0.1;
    Player.Position := NewPosition;

    GamePlay.Player.Hurt(0, Vector3Single(0, -1, 0), 2);
  end;

  procedure TeleportWork(Teleport: T3DTransform; const TeleportBox: TBox3D;
    const Destination: TVector3Single);
  var
    Rot: TVector4Single;
  begin
    Rot := Teleport.Rotation;
    Rot[3] += 0.175 * CompSpeed;
    Teleport.Rotation := Rot;

    if TeleportBox.PointInside(Player.Position) then
    begin
      Player.Position := Destination;
      GamePlay.Player.Camera.CancelFallingDown;

      MainScene.ViewChangedSuddenly;

      SoundEngine.Sound(stTeleport);
    end;
  end;

  procedure SacrilegeAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
  begin
    SoundEngine.Sound(stSacrilegeAmbush);
    for I := 0 to High(SacrilegeAmbushStartingPosition) do
    begin
      CreaturePosition := SacrilegeAmbushStartingPosition[I];
      CreatureDirection := Player.Position - CreaturePosition;
      Ghost.CreateCreature(Items, CreaturePosition, CreatureDirection);
    end;
  end;

  procedure SwordAmbush;
  var
    I: Integer;
    CreaturePosition, CreatureDirection: TVector3Single;
  begin
    for I := 0 to High(SwordAmbushStartingPosition) do
    begin
      CreaturePosition := SwordAmbushStartingPosition[I];
      CreatureDirection := Player.Position - CreaturePosition;
      Ghost.CreateCreature(Items, CreaturePosition, CreatureDirection);
    end;
  end;

const
  { In seconds. }
  CartSoundRepeatTime = 10.0;
begin
  inherited;

  if (Player = nil) or Paused then Exit;

  if FGateExitBox.PointInside(Player.Position) then
  begin
    if GamePlay.Player.Items.FindKind(KeyItemKind) = -1 then
    begin
      Notifications.Show('You need a key to open this door');
      RejectGateExitBox;
    end else
    if GamePlay.Player.Items.FindKind(Sword) = -1 then
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
      FSacrilegeBox.PointInside(Player.Position) then
    begin
      SacrilegeAmbushDone := true;
      SacrilegeAmbush;
    end;

    if (not SwordAmbushDone) and
      (GamePlay.Player.Items.FindKind(Sword) <> -1) then
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

{ TTowerElevatorButton ------------------------------------------------------- }

type
  TTowerElevatorButton = class(TCastlePrecalculatedAnimation)
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TTowerElevatorButton.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

  if Distance > 10 then
    NotificationInteractFailed(
      'You see a button. You''re too far to reach it from here') else
  begin
    { play from the beginning }
    ResetTimeAtLoad;
    TimePlaying := true;
    TTowerLevel(Level).MovingElevator.GoOtherPosition;
  end;
end;

{ TTowerLevel ---------------------------------------------------------------- }

constructor TTowerLevel.Create(
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
  AMenuBackground: boolean);
var
  TowerLevelPath: string;
begin
  inherited;

  TowerLevelPath := CastleLevelsPath + 'tower' + PathDelim;

  Elevator := LoadLevelScene(TowerLevelPath + 'elevator.wrl', true, false);

  ElevatorButton := LoadLevelAnimation(TowerLevelPath + 'elevator_button.kanim', true, false,
    TTowerElevatorButton);

  MovingElevator := T3DLinearMoving.Create(Self);
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
  MovingElevator.CastShadowVolumes := false;
  Items.Add(MovingElevator);
end;

{ TSpiderAppearing ----------------------------------------------------------- }

const
  { Remember to make it -1 lower than actual ceiling geometry,
    otherwise the spiders will be created on the ceiling of the model... }
  SpiderZ = 69.0;

procedure TSpiderAppearing.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Black3Single);
      glBegin(GL_LINES);
        glVertex3f(Translation[0], Translation[1], SpiderZ);
        glVertexv(Translation);
      glEnd;
    glPopAttrib;
  end;

  inherited;
end;

{ TGateExit ------------------------------------------------------------------ }

type
  TGateExit = class(TCastleScene)
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TGateExit.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

  if Distance > 10 then
    NotificationInteractFailed(
      'You see a door. You''re too far to open it from here') else
  begin
    if Player.Items.FindKind(RedKeyItemKind) <> -1 then
    begin
      if (Level.BossCreature <> nil) and (not Level.BossCreature.Dead) then
      begin
        Player.Hurt(2 + Random(5), Vector3Single(0, -1, 0), 2);
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

constructor TCagesLevel.Create(
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
  AMenuBackground: boolean);

  function FindCreatureKind(Kind: TCreatureKind): TCreature;
  var
    I: Integer;
  begin
    for I := 0 to Items.Count - 1 do
      if Items[I] is TCreature then
      begin
        Result := TCreature(Items[I]);
        if Result.Kind = Kind then
          Exit;
      end;
    Result := nil;
  end;

  function FindHintArea(const Id: string): TLevelHintArea;
  var
    I: Integer;
  begin
    for I := 0 to Items.Count - 1 do
      if (Items[I] is TLevelHintArea) and
         (TLevelHintArea(Items[I]).Id = Id) then
        Exit(TLevelHintArea(Items[I]));
    raise Exception.CreateFmt('Level hint area named "%s" not found', [Id]);
  end;

begin
  inherited;

  ThunderEffect := TThunderEffect.Create;

  SpidersAppearing := T3DList.Create(Self);
  Items.Add(SpidersAppearing);
  NextSpidersAppearingTime := 0;

  HintOpenDoor := FindHintArea('HintOpenDoorBox');

  FEndSequence := LoadLevelScene(
    CastleLevelsPath + 'end_sequence' + PathDelim + 'end_sequence_final.wrl',
    true { create octrees },
    true { true: load background of EndSequence; we will use it });
  FEndSequence.Exists := false;
  { Even when FEndSequence will exist, we will not check for collisions
    with it --- no reason to waste time, no collisions will be possible
    as player's move along the EndSequence will be programmed. }
  FEndSequence.Collides := false;
  FEndSequence.CastShadowVolumes := false; { shadow is not visible anyway }
  Items.Add(FEndSequence);

  FGateExit := LoadLevelScene(
    CastleLevelsPath + 'cages' + PathDelim + 'cages_gate_exit.wrl',
    true { create octrees }, false, TGateExit);
  FGateExit.CastShadowVolumes := false; { shadow is not visible anyway }
  Items.Add(FGateExit);

  FBossCreature := FindCreatureKind(SpiderQueen);
end;

procedure TCagesLevel.SetGameWinAnimation(Value: TGameWinAnimation);
begin
  FGameWinAnimation := Value;
  FEndSequence.Exists := GameWinAnimation >= gwaAnimateTo2;
  FGateExit.Exists    := GameWinAnimation <  gwaAnimateTo2;
end;

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
  var
    SA: TSpiderAppearing;
  begin
    SA := TSpiderAppearing.Create(Self);
    SA.Add(Spider.StandAnimation.Scenes[0]);
    SA.Collides := false;
    SA.Translation := Position;
    SpidersAppearing.Add(SA);
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
    Result[0] := Player.Position[0] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[0] := Clamped(Result[0], MinSpiderX, MaxSpiderX);
    Result[1] := Player.Position[1] +
      MapRange(Random, 0.0, 1.0, -RandomDist, RandomDist);
    Result[1] := Clamped(Result[1], MinSpiderY, MaxSpiderY);
    Result[2] := SpiderZ;
  end;

  function CreaturesCount: Cardinal;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Items.Count - 1 do
      if Items[I] is TCreature then
        Inc(Result);
  end;

const
  SpidersFallingSpeed = 25;
  CreaturesCountToAddSpiders = 20;
var
  AboveHeight: Single;
  I: Integer;
  SpiderCreature: TCreature;
  SpiderPosition, SpiderDirection: TVector3Single;
  SpiderMoveDistance: Single;
  SA: TSpiderAppearing;
  TorchLight: PLightInstance;
begin
  inherited;

  if (Player = nil) or Paused then Exit;

  if not GameWin then
  begin
    { Torch light modify, to make an illusion of unstable light }
    TorchLight := MainScene.GlobalLights.FindName('MainHallTorchLight');
    Check(TorchLight <> nil, 'Torch light not found on cages level');
    TorchLight^.Node.FdIntensity.Send(Clamped(
        TorchLight^.Node.FdIntensity.Value +
          MapRange(Random, 0, 1, -5.0, 5.0) * CompSpeed,
        0.5, 1));

    { Maybe appear new spiders }
    if { Spider.Prepared may be false here only if
         --debug-no-creatures was specified. In this case,
         leave Spider unprepared and don't use spider's on this level. }
       Spider.Prepared then
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
        if CreaturesCount < CreaturesCountToAddSpiders then
          for I := 1 to 1 + Random(3) do
            AppearSpider(RandomSpiderXYAroundPlayer);
      end;
    end;

    { Move spiders down }
    I := 0;
    AboveHeight := MaxSingle;
    while I < SpidersAppearing.Count do
    begin
      SA := SpidersAppearing[I] as TSpiderAppearing;
      SpiderPosition := SA.Translation;
      SA.MyHeight(SpiderPosition, AboveHeight);
      if AboveHeight < Spider.Radius * 2 then
      begin
        SpiderDirection :=
          VectorSubtract(Player.Position, SpiderPosition);
        MakeVectorsOrthoOnTheirPlane(SpiderDirection, GravityUp);
        SpiderCreature := Spider.CreateCreature(Items, SpiderPosition, SpiderDirection);
        SpiderCreature.Sound3d(stSpiderAppears, 1.0);
        FreeAndNil(SA); { it will be automatically removed from SpidersAppearing list }
      end else
      begin
        { calculate SpiderMoveDistance }
        SpiderMoveDistance := SpidersFallingSpeed * CompSpeed;
        MinTo1st(SpiderMoveDistance, AboveHeight - Spider.Radius);
        SpiderPosition[2] -= SpiderMoveDistance;
        SA.Translation := SpiderPosition;
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

function TCagesLevel.Background: TBackground;
begin
  if FEndSequence.Exists then
    Result := FEndSequence.Background else
    Result := inherited;
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

procedure TDoomLevelDoor.BeforeTimeIncrease(const NewAnimationTime: TFloatTime);

  function SomethingWillBlockClosingDoor: boolean;
  var
    DoorBox: TBox3D;
    I: Integer;
  begin
    DoorBox := (inherited BoundingBox).Translate(
      GetTranslationFromTime(NewAnimationTime));

    for I := 0 to World.Count - 1 do
      if World[I].Pushable then
      begin
        Result := DoorBox.Collision(World[I].BoundingBox);
        if Result then
          Exit;
      end;
  end;

begin
  inherited;

  { Check the closing doors: if some 3D pushable (player/creature/item)
    will collide after AnimationTime change to NewAnimationTime,
    then we must open door again. }

  if (not EndPosition) and
    (AnimationTime - EndPositionStateChangeTime < MoveTime) and
    SomethingWillBlockClosingDoor then
    RevertGoEndPosition;
end;

procedure TDoomLevelDoor.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  if EndPosition and
    (AnimationTime - EndPositionStateChangeTime >
      MoveTime + StayOpenTime) then
    GoBeginPosition;
end;

function TDoomLevelDoor.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

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
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TElevator9a9b.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
var
  MovingElevator9a9b: T3DLinearMoving;
  Elevator9a9bPickBox: TBox3D;
begin
  Result := Active;
  if not Result then Exit;

  MovingElevator9a9b := (Level as TDoomE1M1Level).MovingElevator9a9b;
  Elevator9a9bPickBox := (Level as TDoomE1M1Level).Elevator9a9bPickBox;

  Result := MovingElevator9a9b.CompletelyBeginPosition and
    Elevator9a9bPickBox.PointInside(Player.Position);

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
    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;
  end;

function TExitButton.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
begin
  Result := Active;
  if not Result then Exit;

  if Distance > 5 then
    NotificationInteractFailed(
      'You''re too far to reach it from here') else
    begin
      SoundEngine.Sound(stDoomExitButton);
      Player.Life := 0;
      (Level as TDoomE1M1Level).ExitMessagePending := true;
    end;
end;

{ TDoomE1M1Level ------------------------------------------------------------- }

constructor TDoomE1M1Level.Create(
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
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
  FakeWall.CastShadowVolumes := false;
  Items.Add(FakeWall);

  Elevator49 := LoadLevelScene(DoomDoorsPathPrefix + 'elevator4_9_final.wrl',
    true { create octrees }, false);

  MovingElevator49 := T3DLinearMoving.Create(Self);
  MovingElevator49.Add(Elevator49);
  MovingElevator49.MoveTime := 3.0;
  MovingElevator49.TranslationEnd := Vector3Single(0, 0, -6.7);
  MovingElevator49.SoundGoEndPosition := stElevator;
  MovingElevator49.SoundGoEndPositionLooping := true;
  MovingElevator49.SoundGoBeginPosition := stElevator;
  MovingElevator49.SoundGoBeginPositionLooping := true;
  MovingElevator49.SoundTracksCurrentPosition := true;
  MovingElevator49.CastShadowVolumes := false;
  Items.Add(MovingElevator49);

  Elevator9a9b := LoadLevelScene(DoomDoorsPathPrefix + 'elevator_9a_9b_final.wrl',
    true { create octrees }, false, TElevator9a9b);

  MovingElevator9a9b := T3DLinearMoving.Create(Self);
  MovingElevator9a9b.Add(Elevator9a9b);
  MovingElevator9a9b.MoveTime := 3.0;
  MovingElevator9a9b.TranslationEnd := Vector3Single(0, 0, -7.5);
  MovingElevator9a9b.SoundGoEndPosition := stElevator;
  MovingElevator9a9b.SoundGoEndPositionLooping := true;
  MovingElevator9a9b.SoundGoBeginPosition := stElevator;
  MovingElevator9a9b.SoundGoBeginPositionLooping := true;
  MovingElevator9a9b.SoundTracksCurrentPosition := true;
  MovingElevator9a9b.CastShadowVolumes := false;
  Items.Add(MovingElevator9a9b);

  ExitButton := LoadLevelScene(DoomDoorsPathPrefix + 'exit_button_final.wrl',
    true { create octrees }, false, TExitButton);
  ExitButton.CastShadowVolumes := false;
  Items.Add(ExitButton);
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

  if (Player = nil) or Paused then Exit;

  if MovingElevator49.CompletelyBeginPosition and
     Elevator49DownBox.PointInside(Player.Position) then
  begin
    MovingElevator49.GoEndPosition;
  end;

  if MovingElevator9a9b.CompletelyEndPosition and
     (AnimationTime - MovingElevator9a9b.EndPositionStateChangeTime >
       MovingElevator9a9b.MoveTime +
       { This is the time for staying in lowered position. }
       2.0) then
    MovingElevator9a9b.GoBeginPosition;

  if ExitMessagePending and (not GamePlay.Player.Camera.FallingOnTheGround) then
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
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
  AMenuBackground: boolean);
var
  Water: TCastlePrecalculatedAnimation;
begin
  inherited;

  Water := LoadLevelAnimation(CastleLevelsPath + 'gate_background' +
    PathDelim + 'water.kanim', false, false);
  Water.CastShadowVolumes := false; { water shadow would look awkward }
  { No octrees created for water (because in normal usage, player will not
    walk on this level). For safety, Collides set to @false, in case
    user enters this level by debug menu. }
  Water.Collides := false;
  Items.Add(Water);

  Water.TimePlaying := true;
end;

{ TFountainLevel ------------------------------------------------------------- }

constructor TFountainLevel.Create(
  const AId: string;
  const ASceneFileName: string;
  const ATitle: string; const ATitleHint: string; const ANumber: Integer;
  DOMElement: TDOMElement;
  AResources: T3DResourceList;
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
    AttributesSet(Fountain.Attributes);
    Progress.Init(Fountain.PrepareResourcesSteps, 'Loading water');
    try
      Fountain.PrepareResources([prRender, prBoundingBox], true, BaseLights,
        1 { MultiSampling = 1, it is ignored as we do not include prScreenEffects });
    finally Progress.Fini end;
    Fountain.FreeResources([frTextureDataInNodes]);
    Fountain.CastShadowVolumes := false; { not manifold }
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

initialization
  { if for some reason (uses clauses) GameLevel is not initialized yet
    --- the initialize it now }
  if LevelClasses = nil then
    LevelClasses := TLevelClasses.Create;

  LevelClasses['Level'] := TLevel;
  LevelClasses['Cages'] := TCagesLevel;
  LevelClasses['Gate'] := TGateLevel;
  LevelClasses['GateBackground'] := TGateBackgroundLevel;
  LevelClasses['CastleHall'] := TCastleHallLevel;
  LevelClasses['DoomE1M1'] := TDoomE1M1Level;
  LevelClasses['Tower'] := TTowerLevel;
  LevelClasses['Fountain'] := TFountainLevel;
end.
