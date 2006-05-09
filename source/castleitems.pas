{
  Copyright 2006 Michalis Kamburelis.

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
unit CastleItems;

interface

uses Boxes3d, VRMLNodes, VRMLFlatSceneGL, VectorMath, KambiUtils,
  KambiClassUtils, Images, OpenGLh, CastleSound,
  VRMLGLAnimation, VRMLGLAnimationInfo, CastleObjectKinds;

{$define read_interface}

type
  TItem = class;

  { Kind of item.

    Design question: Maybe it's better making TSword a descendant of TItem,
    and not creating TItemKind class ? This seems somewhat cleaner
    from OOP approach. Then various functions/properties
    of TItemKind must be handled as "class function" of TItem.
    Answer: I once did this (see "Jamy & Nory"), but it turns out that it's
    not comfortable --- for example I would need associative array
    (like LoadedModels) to keep TItemKind.FScene value
    (to not load and not construct new GL display list each time
    I create TSword instance).

    Note that this unit handles all destruction of TItemKind instances
    (and also creates most (all, for now) instances of TItemKind).
    You're free to create new descendants and instances of TItemKind
    outside this unit, but then leave freeing them to this unit. }
  TItemKind = class(TObjectKind)
  private
    FModelFileName: string;
    FScene: TVRMLFlatSceneGL;
    FName: string;
    FImageFileName: string;
    FImage: TImage;
    FGLList_DrawImage: TGLuint;
    FBoundingBoxRotated: TBox3d;
    FBoundingBoxRotatedCalculated: boolean;
  public
    constructor Create(const AModelFileName, AVRMLNodeName, AName,
      AImageFileName: string);
    destructor Destroy; override;

    property ModelFileName: string read FModelFileName;

    { Nice name for user. }
    property Name: string read FName;

    { Note that the first call to Scene will try to load model from
      ModelFileName if it wasn't already loaded. }
    function Scene: TVRMLFlatSceneGL;

    { This is a 2d image, to be used for inventory slots etc.
      When you call this for the 1st time, the image will be loaded
      from ImageFileName.

      @noAutoLinkHere }
    function Image: TImage;

    property ImageFileName: string read FImageFileName;

    { This is OpenGL display list to draw @link(Image). }
    function GLList_DrawImage: TGLuint;

    { Use this item.

      In this class, this just prints a message "this item cannot be used".

      Implementation of this method can assume that Item is one of
      player's owned Items. Implementation of this method can change
      Item instance properties, including Quantity.
      As a very special exception, implementation of this method
      is allowed to set Quantity of Item to 0.

      Never call this method when Player.Dead. Implementation of this
      method may assume that Player is not Dead.

      Caller of this method should always be prepared to immediately
      handle the "Quantity = 0" situation by freeing given item,
      removing it from any list etc. }
    procedure Use(Item: TItem); virtual;

    { This returns Scene.BoundingBox enlarged a little (along X and Y)
      to account the fact that Scene may be rotated around +Z vector. }
    function BoundingBoxRotated: TBox3d;

    procedure PrepareRender; override;
    function PrepareRenderSteps: Cardinal; override;
    procedure CloseGL; override;
  end;

  TObjectsListItem_3 = TItemKind;
  {$I objectslist_3.inc}
  TItemKindsList = class(TObjectsList_3)
    { Calls PrepareRender for all items.
      This does Progress.Init, Step, Fini. }
    procedure PrepareRender;
  end;

  TItemPotionOfLifeKind = class(TItemKind)
    procedure Use(Item: TItem); override;
  end;

  TItemWeaponKind = class(TItemKind)
  private
    FScreenImageFileName: string;
    FScreenImage: TImage;
    FGLList_DrawScreenImage: TGLuint;
    FScreenImageAlignLeft: boolean;
    FScreenImageAlignBottom : boolean;
    FEquippingSound: TSoundType;
    FAttackAnimation: TVRMLGLAnimation;
    FAttackAnimationInfo: TVRMLGLAnimationInfo;
    FActualAttackTime: Single;
    FSoundAttackStart: TSoundType;
  public
    { Constryctor. You can pass
      AAttackAnimationInfo = nil to get
      AAttackAnimation = nil. }
    constructor Create(const AModelFileName, AVRMLNodeName, AName,
      AImageFileName, AScreenImageFileName: string;
      AScreenImageAlignLeft, AScreenImageAlignBottom: boolean;
      AEquippingSound: TSoundType;
      AAttackAnimationInfo: TVRMLGLAnimationInfo);
    destructor Destroy; override;

    { Because whole screen is quite large, we want our ScreenImage
      to be much smaller. So I can't simply cover whole screen with
      ScreenImage --- instead these properties tell me where to
      put the ScreenImage on the screen. }
    property ScreenImageAlignLeft: boolean read FScreenImageAlignLeft;
    property ScreenImageAlignBottom: boolean read FScreenImageAlignBottom;

    { "Screen image" will be displayed on player's screen when this
      item will be equipped.

      GLList_DrawScreenImage will draw the image temporarily
      turning on alpha test.
      It will be displayed with alpha test,
      so it's OK to use sharp alpha channel on this image.

      GLList_DrawScreenImage changes raster position and current matrix.

      @groupBegin }
    property ScreenImageFileName: string read FScreenImageFileName;
    function ScreenImage: TImage;
    function GLList_DrawScreenImage: TGLuint;
    { @groupEnd }

    { Sound to make on equipping. Each weapon can have it's own
      equipping sound. }
    property EquippingSound: TSoundType read FEquippingSound;

    { This is an animation of attack with this weapon.
      TimeBegin must be 0. }
    property AttackAnimation: TVRMLGLAnimation
      read FAttackAnimation;

    procedure Use(Item: TItem); override;

    procedure PrepareRender; override;
    function PrepareRenderSteps: Cardinal; override;
    procedure CloseGL; override;

    { This is the time point within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime default 0.0;

    { Perform real attack here.
      This may mean hurting some creature within the range,
      or shooting some missile. You can also play some sound here. }
    procedure ActualAttack(Item: TItem); virtual; abstract;

    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;
  end;

  TItemSwordKind = class(TItemWeaponKind)
  public
    procedure ActualAttack(Item: TItem); override;
  end;

  TItemBowKind = class(TItemKind)
  public
//    procedure ActualAttack(Item: TItem); override;
  end;

  TItemScrollOfFlyingKind = class(TItemKind)
    procedure Use(Item: TItem); override;
  end;

  { An item. Actually, this represents a collection of
    "stacked" items that have the same properties --- see Quantity property. }
  TItem = class
  private
    FKind: TItemKind;
    FQuantity: Cardinal;
  public
    constructor Create(AKind: TItemKind; AQuantity: Cardinal);

    property Kind: TItemKind read FKind;

    { Quantity of this item.
      This must always be >= 1. }
    property Quantity: Cardinal read FQuantity write FQuantity;

    { Stackable means that two items are equal and they can be summed
      into one item by adding their Quantity values.
      Practially this means that all properties
      of both items are equal, with the exception of Quantity. }
    function Stackable(Item: TItem): boolean;

    { This splits item (with Quantity >= 2) into two items.
      It returns newly created object with the same properties
      as this object, and with Quantity set to QuantitySplit.
      And it lowers our Quantity by QuantitySplit.

      Always QuantitySplit must be >= 1 and < Quantity. }
    function Split(QuantitySplit: Cardinal): TItem;
  end;

  TObjectsListItem_2 = TItem;
  {$I objectslist_2.inc}
  TItemsList = class(TObjectsList_2)
  public
    { This checks is Item "stackable" with any item on the list.
      Returns index of item on the list that is stackable with given Item,
      or -1 if none. }
    function Stackable(Item: TItem): Integer;

    { Searches for item of given Kind. Returns index of first found,
      or -1 if not found. }
    function FindKind(Kind: TItemKind): Integer;
  end;

  TItemOnLevel = class
  private
    FItem: TItem;
    FPosition: TVector3Single;
    { Rotation around Z }
    FRotation: Single;
  public
    constructor Create(AItem: TItem; const APosition: TVector3Single);
    destructor Destroy; override;

    { Note that this Item is owned by TItemOnLevel instance,
      so when you will free this TItemOnLevel instance,
      Item will be also freed.
      However, you can prevent that if you want --- see ExtractItem. }
    property Item: TItem read FItem;

    { This returns Item and sets Item to nil.
      This is the only way to force TItemOnLevel instance
      to *not* free associated Item object on destruction.

      Note that Item = nil is considered invalid state of this object,
      and the only thing that you should do further with this
      TItemOnLevel instance is to free it ! }
    function ExtractItem: TItem;

    { This is the position of the (0, 0, 0) point of item model. }
    property Position: TVector3Single read FPosition;

    { Tests collision of this item with Player (in @link(Player) variable). }
    function PlayerCollision: boolean;

    { Render the item, on current Position with current rotation etc.
      Current matrix should be modelview, this pushes/pops matrix state
      (so it 1. needs one place on matrix stack,
      2. doesn't modify current matrix).

      Pass current viewing Frustum to allow optimizing this
      (when item for sure is not within Frustum, we don't have
      to push it to OpenGL). }
    procedure Render(const Frustum: TFrustum);

    procedure Idle(const CompSpeed: Single);

    { This returns BoundingBox of this item, taking into account
      it's current Position and the fact that items constantly rotate
      (around local +Z). So it's actually Item.Kind.Scene.BoundingBox translated
      and enlarged as appropriate. }
    function BoundingBox: TBox3d;

    { Call this when user clicked on the item.
      This will do some GameMessage describing the item
      (may depend on Distance --- when it's too far, player may not
      be able to exactly tell what the item is). }
    procedure ItemPicked(const Distance: Single);
  end;

  TObjectsListItem_1 = TItemOnLevel;
  {$I objectslist_1.inc}
  TItemsOnLevelList = class(TObjectsList_1)
    { Call Render for all items. }
    procedure Render(const Frustum: TFrustum; const Transparent: boolean);
    { Call Idle for all items. }
    procedure Idle(const CompSpeed: Single);
    { Check collision with all items, returns index of first collider
      (or -1 if no collision). }
    function PlayerCollision: Integer;
  end;

var
  ItemsKinds: TItemKindsList;

  Sword: TItemSwordKind;
  Bow: TItemBowKind;
  LifePotion: TItemKind;
  ScrollOfFlying: TItemKind;
  KeyItemKind: TItemKind;
  RedKeyItemKind: TItemKind;
  Quiver: TItemKind;

{ Returns nil if not found. }
function ItemKindWithVRMLNodeName(const VRMLNodeName: string): TItemKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, Object3dAsVRML, GLWindow, CastleWindow,
  KambiGLUtils, CastlePlay, KambiFilesUtils, ProgressUnit,
  CastleCreatures, CastleVideoOptions, CastleTimeMessages;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(const AModelFileName, AVRMLNodeName, AName,
  AImageFileName: string);
begin
  inherited Create(AVRMLNodeName);
  FModelFileName := AModelFileName;
  FName := AName;
  FImageFileName := AImageFileName;
  ItemsKinds.Add(Self);
end;

destructor TItemKind.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FScene);
  inherited;
end;

function TItemKind.Scene: TVRMLFlatSceneGL;
begin
  if FScene = nil then
  begin
    FScene := TVRMLFlatSceneGL.Create(LoadAsVRML(
      ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'models' + PathDelim + ModelFileName, false),
      true, roSceneAsAWhole);
  end;
  Result := FScene;
end;

function TItemKind.Image: TImage;
begin
  if FImage = nil then
    FImage := LoadImage(
      ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'images' + PathDelim + ImageFileName, [], []);
  Result := FImage;
end;

function TItemKind.GLList_DrawImage: TGLuint;
begin
  if FGLList_DrawImage = 0 then
    FGLList_DrawImage := ImageDrawToDispList(Image);
  Result := FGLList_DrawImage;
end;

procedure TItemKind.Use(Item: TItem);
begin
  TimeMessage('This item cannot be used');
end;

function TItemKind.BoundingBoxRotated: TBox3d;
var
  HorizontalSize: Single;
begin
  if not FBoundingBoxRotatedCalculated then
  begin
    FBoundingBoxRotated := Scene.BoundingBox;

    { Note that I *cannot* assume below that Scene.BoundingBox
      middle point is (0, 0, 0). So I just take the largest distance
      from point (0, 0) to any corner of the Box (distance 2D,
      only horizontally) and this tells me the horizontal sizes of the
      bounding box.

      This hurts a little (because of 1 call to Sqrt),
      that's why results of this function are cached if FBoundingBoxRotated. }
    HorizontalSize := Max(Max(
      VectorLenSqr(Vector2Single(FBoundingBoxRotated[0, 0], FBoundingBoxRotated[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated[1, 0], FBoundingBoxRotated[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated[1, 0], FBoundingBoxRotated[1, 1]))),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated[0, 0], FBoundingBoxRotated[1, 1])));
    HorizontalSize := Sqrt(HorizontalSize);
    FBoundingBoxRotated[0, 0] := -HorizontalSize;
    FBoundingBoxRotated[0, 1] := -HorizontalSize;
    FBoundingBoxRotated[1, 0] := +HorizontalSize;
    FBoundingBoxRotated[1, 1] := +HorizontalSize;

    FBoundingBoxRotatedCalculated := true;
  end;
  Result := FBoundingBoxRotated;
end;

procedure TItemKind.PrepareRender;
begin
  Scene.PrepareRender(false, true, false, false);
  Progress.Step;
end;

function TItemKind.PrepareRenderSteps: Cardinal;
begin
  Result := 1;
end;

procedure TItemKind.CloseGL;
begin
  if FScene <> nil then
    FScene.CloseGL;
end;

{ TItemKindsList ------------------------------------------------------------- }

procedure TItemKindsList.PrepareRender;
var
  I: Integer;
  PrepareRenderSteps: Cardinal;
begin
  PrepareRenderSteps := 0;
  for I := 0 to High do
    PrepareRenderSteps +=  Items[I].PrepareRenderSteps;

  Progress.Init(PrepareRenderSteps, 'Loading items');
  try
    for I := 0 to High do
      Items[I].PrepareRender;
  finally Progress.Fini; end;
end;

{ TItemPotionOfLifeKind ---------------------------------------------------- }

procedure TItemPotionOfLifeKind.Use(Item: TItem);
begin
  if Player.Life < Player.MaxLife then
  begin
    Player.Life := Min(Player.Life + 50, Player.MaxLife);
    TimeMessage(Format('You drink "%s"', [Item.Kind.Name]));
    Item.Quantity := Item.Quantity - 1;
    Sound(stPlayerPotionDrink);
  end else
    TimeMessage('You feel quite alright, no need to waste this potion');
end;

{ TItemWeaponKind ------------------------------------------------------------ }

constructor TItemWeaponKind.Create(const AModelFileName, AVRMLNodeName, AName,
  AImageFileName, AScreenImageFileName: string;
  AScreenImageAlignLeft, AScreenImageAlignBottom: boolean;
  AEquippingSound: TSoundType;
  AAttackAnimationInfo: TVRMLGLAnimationInfo);
begin
  inherited Create(AModelFileName, AVRMLNodeName, AName,
    AImageFileName);
  FScreenImageFileName := AScreenImageFileName;
  FScreenImageAlignLeft := AScreenImageAlignLeft;
  FScreenImageAlignBottom := AScreenImageAlignBottom;
  FEquippingSound := AEquippingSound;
  FAttackAnimationInfo := AAttackAnimationInfo;
end;

destructor TItemWeaponKind.Destroy;
begin
  FreeAndNil(FAttackAnimation);
  FreeAndNil(FAttackAnimationInfo);
  FreeAndNil(FScreenImage);
  inherited;
end;

function TItemWeaponKind.ScreenImage: TImage;
begin
  if FScreenImage = nil then
    FScreenImage := LoadImage(
      ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'equipped' + PathDelim + ScreenImageFileName, [], []);
  Result := FScreenImage;
end;

function TItemWeaponKind.GLList_DrawScreenImage: TGLuint;
var
  X, Y: Integer;
begin
  if FGLList_DrawScreenImage = 0 then
  begin
    FGLList_DrawScreenImage := glGenListsCheck(1, 'TItemWeaponKind.GLList_DrawScreenImage');
    glNewList(FGLList_DrawScreenImage, GL_COMPILE);
    try
      if ScreenImageAlignLeft then
        X := 0 else
        X := RequiredScreenWidth - ScreenImage.Width;
      if ScreenImageAlignBottom then
        Y := 0 else
        Y := RequiredScreenHeight - ScreenImage.Height;

      glLoadIdentity;
      glRasterPos2i(X, Y);

      glPushAttrib(GL_COLOR_BUFFER_BIT);
        glAlphaFunc(GL_GREATER, 0.5);
        glEnable(GL_ALPHA_TEST);
          ImageDraw(ScreenImage);
      glPopAttrib;
    finally glEndList end;
  end;
  Result := FGLList_DrawScreenImage;
end;

procedure TItemWeaponKind.Use(Item: TItem);
begin
  Player.EquippedWeapon := Item;
end;

procedure TItemWeaponKind.PrepareRender;

  procedure CreateIfNeeded(var Anim: TVRMLGLAnimation;
    AnimInfo: TVRMLGLAnimationInfo);
  begin
    if (AnimInfo <> nil) and (Anim = nil) then
      Anim := AnimInfo.CreateAnimation;
    Progress.Step;
    if Anim <> nil then
      Anim.PrepareRender(false, true, false, false, false);
    Progress.Step;
  end;

begin
  inherited;
  CreateIfNeeded(FAttackAnimation, FAttackAnimationInfo);
end;

function TItemWeaponKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TItemWeaponKind.CloseGL;
begin
  inherited;
  if AttackAnimation <> nil then AttackAnimation.CloseGL;
end;

{ TItemSwordKind ------------------------------------------------------------- }

procedure TItemSwordKind.ActualAttack(Item: TItem);
var
  WeaponBoundingBox: TBox3d;
  I: Integer;
  C: TCreature;
begin
  WeaponBoundingBox := Box3dTranslate(Player.BoundingBox,
    VectorAdjustToLength(Player.Navigator.CameraDir, 1.0));
  { Tests: Writeln('WeaponBoundingBox is ', Box3dToNiceStr(WeaponBoundingBox)); }
  for I := 0 to Level.Creatures.High do
  begin
    C := Level.Creatures[I];
    { Tests: Writeln('Creature bbox is ', Box3dToNiceStr(C.BoundingBox)); }
    if Boxes3dCollision(C.BoundingBox, WeaponBoundingBox) then
    begin
      C.Life := C.Life - 20 - Random(20);
      C.LastAttackDirection := Player.Navigator.CameraDir;
    end;
  end;
end;

{ TItemBowKind ------------------------------------------------------------- }

(*procedure TItemBowKind.ActualAttack(Item: TItem);
begin
  { TODO }
end;*)

{ TItemScrollOfFlyingKind ---------------------------------------------------- }

procedure TItemScrollOfFlyingKind.Use(Item: TItem);
begin
  TimeMessage(Format('You cast spell from "%s"', [Item.Kind.Name]));
  Player.FlyingModeTimeoutBegin(30.0);
  Item.Quantity := Item.Quantity - 1;
  Sound(stPlayerCastFlyingSpell);
end;

{ TItem ------------------------------------------------------------ }

constructor TItem.Create(AKind: TItemKind; AQuantity: Cardinal);
begin
  inherited Create;
  FKind := AKind;
  FQuantity := AQuantity;
  Assert(Quantity >= 1, 'Item''s Quantity must be >= 1');
end;

function TItem.Stackable(Item: TItem): boolean;
begin
  Result := Item.Kind = Kind;
end;

function TItem.Split(QuantitySplit: Cardinal): TItem;
begin
  Check(Between(Integer(QuantitySplit), 1, Quantity - 1),
    'You must split >= 1 and less than current Quantity');

  Result := TItem.Create(Kind, QuantitySplit);

  FQuantity -= QuantitySplit;
end;

{ TItemsList ------------------------------------------------------------ }

function TItemsList.Stackable(Item: TItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Stackable(Item) then
      Exit;
  Result := -1;
end;

function TItemsList.FindKind(Kind: TItemKind): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Kind = Kind then
      Exit;
  Result := -1;
end;

{ TItemOnLevel ------------------------------------------------------------ }

constructor TItemOnLevel.Create(AItem: TItem; const APosition: TVector3Single);
begin
  inherited Create;
  FItem := AItem;
  FPosition := APosition;
end;

destructor TItemOnLevel.Destroy;
begin
  FreeAndNil(FItem);
  inherited;
end;

function TItemOnLevel.ExtractItem: TItem;
begin
  Result := Item;
  FItem := nil;
end;

function TItemOnLevel.PlayerCollision: boolean;
begin
  { I approximate both player and item as a bounding boxes.
    This works quite good, and is "precise enough" :)
    Note: remember to change TItemsOnLevelList.PlayerCollision
    accordingly if I ever change implementation of this. }

  Result := Boxes3dCollision(BoundingBox, Player.BoundingBox);
end;

procedure TItemOnLevel.Render(const Frustum: TFrustum);
begin
  if FrustumBox3dCollisionPossibleSimple(Frustum, BoundingBox) then
  begin
    glPushMatrix;
      glTranslatev(Position);
      glRotatev(FRotation, UnitVector3Single[2]);
      Item.Kind.Scene.Render(nil);
    glPopMatrix;

    if RenderBoundingBoxes then
    begin
      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
        glColorv(Gray3Single);
        DrawGLBoxWire(BoundingBox, 0, 0, 0, true);
      glPopAttrib;
    end;
  end;
end;

procedure TItemOnLevel.Idle(const CompSpeed: Single);
const
  PositionRadius = 1.0;
  FallingDownSpeed = 0.2;
var
  IsAboveTheGround: boolean;
  SqrHeightAboveTheGround, HeightAboveTheGround: Single;
  ShiftedPosition: TVector3Single;
  ProposedNewShiftedPosition, NewShiftedPosition: TVector3Single;
begin
  FRotation += 3 * CompSpeed;

  ShiftedPosition := Position;
  ShiftedPosition[2] += PositionRadius;

  { Note that I'm using ShiftedPosition, not Position,
    and later I'm comparing "SqrHeightAboveTheGround > Sqr(PositionRadius)",
    instead of "SqrHeightAboveTheGround > 0".
    Otherwise, I risk that when item will be placed perfectly on the ground,
    it may "slip through" this ground down.

    For the same reason, I use sphere around ShiftedPosition
    when doing Level.MoveAllowed below. }

  Level.GetCameraHeight(ShiftedPosition, IsAboveTheGround,
    SqrHeightAboveTheGround);
  if (not IsAboveTheGround) or
     (SqrHeightAboveTheGround > Sqr(PositionRadius)) then
  begin
    { Item falls down because of gravity. }

    HeightAboveTheGround := Sqrt(SqrHeightAboveTheGround);
    ProposedNewShiftedPosition := ShiftedPosition;
    ProposedNewShiftedPosition[2] -= Min(
      HeightAboveTheGround - PositionRadius,
      CompSpeed * FallingDownSpeed);

    if Level.MoveAllowed(ShiftedPosition, ProposedNewShiftedPosition,
      NewShiftedPosition, true, PositionRadius) then
    begin
      FPosition := ProposedNewShiftedPosition;
      FPosition[2] -= PositionRadius;
    end;
  end;
end;

function TItemOnLevel.BoundingBox: TBox3d;
begin
  Result := Box3dTranslate(Item.Kind.BoundingBoxRotated, Position);
end;

procedure TItemOnLevel.ItemPicked(const Distance: Single);
const
  VisibleItemDistance = 60.0;
var
  S: string;
begin
  if Distance <= VisibleItemDistance then
  begin
    S := Format('You see an item "%s"', [Item.Kind.Name]);
    if Item.Quantity <> 1 then
      S += Format(' (quantity %d)', [Item.Quantity]);
    TimeMessage(S);
  end else
    TimeMessage('You see some item, but it''s too far to tell exactly what it is');
end;

{ TItemsOnLevelList -------------------------------------------------- }

procedure TItemsOnLevelList.Render(const Frustum: TFrustum;
  const Transparent: boolean);
var
  I: Integer;
begin
  for I := 0 to High do
    if Items[I].Item.Kind.Transparent = Transparent then
      Items[I].Render(Frustum);
end;

procedure TItemsOnLevelList.Idle(const CompSpeed: Single);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Idle(CompSpeed);
end;

function TItemsOnLevelList.PlayerCollision: Integer;
var
  PlayerBoundingBox: TBox3d;
begin
  { Instead of just calling TItemOnLevel.PlayerCollision,
    this is optimized a little: it calculates Player.BoundingBox
    only once. }
  PlayerBoundingBox := Player.BoundingBox;
  for Result := 0 to High do
    if Boxes3dCollision(Items[Result].BoundingBox, PlayerBoundingBox) then
      Exit;
  Result := -1;
end;

{ other global stuff --------------------------------------------------- }

function ItemKindWithVRMLNodeName(const VRMLNodeName: string): TItemKind;
var
  I: Integer;
begin
  for I := 0 to ItemsKinds.Count - 1 do
  begin
    Result := ItemsKinds.Items[I];
    if Result.VRMLNodeName = VRMLNodeName then
      Exit;
  end;
  Result := nil;
end;

{ initialization / finalization ---------------------------------------- }

procedure GLWindowClose(Glwin: TGLWindow);
var
  I: Integer;
begin
  { In fact, ItemsKinds will always be nil here, because
    GLWindowClose will be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on ItemsKinds, and this implicitly did CloseGL,
    so everything is OK. }

  if ItemsKinds <> nil then
  begin
    for I := 0 to ItemsKinds.Count - 1 do
      ItemsKinds[I].CloseGL;
  end;
end;

procedure DoInitialization;

  function WeaponAnimFileName(const ModelFileName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'attack_animations' + PathDelim + ModelFileName;
  end;

begin
  Glw.OnCloseList.AppendItem(@GLWindowClose);

  ItemsKinds := TItemKindsList.Create;

  Sword := TItemSwordKind.Create('sword.wrl', 'Sword', 'Sword', 'sword.png',
    'sword.png', false, true, stSwordEquipping,
    TVRMLGLAnimationInfo.Create(
      [ WeaponAnimFileName('sword_1.wrl'),
        WeaponAnimFileName('sword_2.wrl') ],
      [ 0, 0.5 ],
      30, roSceneAsAWhole, false, false));
  Sword.ActualAttackTime := MapRange(0.0, -1.0, +0.7, 0.0, 0.5);
  Sword.SoundAttackStart := stSwordAttackStart;

  Bow := TItemBowKind.Create('bow.wrl', 'Bow', 'Bow', 'bow.png'
    {,
    'bow.png', false, true, stBowEquipping,
    TVRMLGLAnimationInfo.Create(
      [ WeaponAnimFileName('bow_equipped.wrl')
      [ 0 ],
      1, roSceneAsAWhole, false, false)});
{  Bow.ActualAttackTime := 0;
  Bow.SoundAttackStart := stBowAttackStart;}

  LifePotion := TItemPotionOfLifeKind.Create('life_potion_processed.wrl',
    'LifePotion', 'Potion of Life', 'life_potion.png');
  LifePotion.Transparent := true;

  ScrollOfFlying := TItemScrollOfFlyingKind.Create('scroll_final.wrl',
    'ScrFlying', 'Scroll Of Flying', 'scroll.png');

  KeyItemKind := TItemKind.Create('key.wrl',
    'Key', 'Key', 'key.png');

  RedKeyItemKind := TItemKind.Create('red_key.wrl',
    'RedKey', 'Red Key', 'red_key.png');

  Quiver := TItemKind.Create('quiver.wrl',
    'Quiver', 'Quiver', 'quiver.png');
end;

procedure DoFinalization;
begin
  FreeWithContentsAndNil(ItemsKinds);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.