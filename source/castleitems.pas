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
unit CastleItems;

interface

uses Boxes3D, VRMLNodes, VRMLGLScene, VectorMath, KambiUtils,
  KambiClassUtils, Classes, Images, GL, GLU, KambiGLUtils, CastleSound,
  VRMLGLAnimation, VRMLGLAnimationInfo, CastleObjectKinds,
  KambiXMLConfig, XmlSoundEngine, Frustum, Base3D;

const
  DefaultItemDamageConst = 5.0;
  DefaultItemDamageRandom = 5.0;
  DefaultItemActualAttackTime = 0.0;

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
    outside this unit, but then leave freeing them to this unit.

    Note that after creating an instance of this item,
    before you do anything else, you have to initialize some of it's
    properties by calling LoadFromFile. In this class, this initializes
    it's ModelFileName, Name amd ImageFileName --- in descendants, more
    required properties may be initialized here. }
  TItemKind = class(TObjectKind)
  private
    FModelFileName: string;
    FScene: TVRMLGLScene;
    FName: string;
    FImageFileName: string;
    FImage: TImage;
    FGLList_DrawImage: TGLuint;
    FBoundingBoxRotated: TBox3D;
    FBoundingBoxRotatedCalculated: boolean;
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;

    { This is like @inherited, but it passes proper values for boolean parameters
      specifying what to prepare. }
    procedure CreateAnimationIfNeeded(
      const AnimationName: string;
      var Anim: TVRMLGLAnimation;
      AnimInfo: TVRMLGLAnimationInfo;
      TransparentGroups: TTransparentGroups;
      const BaseLights: TLightInstancesList);

    function GetStringCheckNonEmpty(KindsConfig: TKamXMLConfig;
      const AttrName: string): string;
  public
    { The constructor. }
    constructor Create(const AVRMLNodeName: string);
    destructor Destroy; override;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;

    property ModelFileName: string read FModelFileName;

    { Nice name for user. }
    property Name: string read FName;

    { Note that the Scene is nil if not PrepareRenderDone. }
    function Scene: TVRMLGLScene;

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
    function BoundingBoxRotated: TBox3D;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;
    procedure GLContextClose; override;
  end;

  TObjectsListItem_3 = TItemKind;
  {$I objectslist_3.inc}
  TItemKindsList = class(TObjectsList_3)
  public
    { Calls PrepareRender for all items.
      This does Progress.Init, Step, Fini. }
    procedure PrepareRender(const BaseLights: TLightInstancesList);

    { Call FreePrepareRender for all items. }
    procedure FreePrepareRender;

    { This opens items/kinds.xml file and calls LoadFromFile for
      all existing TItemKind instances. }
    procedure LoadFromFile;
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
  protected
    procedure PrepareRenderInternal(const BaseLights: TLightInstancesList); override;
  public
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
    property EquippingSound: TSoundType
      read FEquippingSound write FEquippingSound;

    { This is an animation of attack with this weapon.
      TimeBegin must be 0. }
    property AttackAnimation: TVRMLGLAnimation
      read FAttackAnimation;

    procedure Use(Item: TItem); override;

    function PrepareRenderSteps: Cardinal; override;
    procedure FreePrepareRender; override;
    procedure GLContextClose; override;

    { This is the time point within AttackAnimation
      at which ActualAttack method will be called.
      Note that actually ActualAttack may be called a *very little* later
      (hopefully it shouldn't be noticeable to the player). }
    property ActualAttackTime: Single
      read FActualAttackTime write FActualAttackTime
      default DefaultItemActualAttackTime;

    { Perform real attack here.
      This may mean hurting some creature within the range,
      or shooting some missile. You can also play some sound here. }
    procedure ActualAttack(Item: TItem); virtual; abstract;

    property SoundAttackStart: TSoundType
      read FSoundAttackStart write FSoundAttackStart default stNone;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;
  end;

  TItemShortRangeWeaponKind = class(TItemWeaponKind)
  private
    FDamageConst: Single;
    FDamageRandom: Single;
  public
    constructor Create(const AVRMLNodeName: string);

    property DamageConst: Single read FDamageConst write FDamageConst
      default DefaultItemDamageConst;
    property DamageRandom: Single read FDamageRandom write FDamageRandom
      default DefaultItemDamageRandom;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); override;
  end;

  TItemSwordKind = class(TItemShortRangeWeaponKind)
  public
    procedure ActualAttack(Item: TItem); override;
  end;

  TItemBowKind = class(TItemWeaponKind)
  public
    procedure ActualAttack(Item: TItem); override;
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
    property Position: TVector3Single read FPosition write FPosition;

    { Tests collision of this item with Player (in @link(Player) variable). }
    function PlayerCollision: boolean;

    { Render the item, on current Position with current rotation etc.
      Current matrix should be modelview, this pushes/pops matrix state
      (so it 1. needs one place on matrix stack,
      2. doesn't modify current matrix).

      Pass current viewing Frustum to allow optimizing this
      (when item for sure is not within Frustum, we don't have
      to push it to OpenGL). }
    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams);

    procedure Idle(const CompSpeed: Single);

    { This returns BoundingBox of this item, taking into account
      it's current Position and the fact that items constantly rotate
      (around local +Z). So it's actually Item.Kind.Scene.BoundingBox translated
      and enlarged as appropriate. }
    function BoundingBox: TBox3D;

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
    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams);
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

uses SysUtils, GLWindow, CastleWindow,
  CastlePlay, KambiFilesUtils, ProgressUnit,
  CastleCreatures, CastleVideoOptions, CastleNotifications,
  VRMLScene, VRMLTriangle, GLImages;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}
{$I objectslist_3.inc}

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(const AVRMLNodeName: string);
begin
  inherited Create(AVRMLNodeName);
  ItemsKinds.Add(Self);
end;

destructor TItemKind.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FScene);
  inherited;
end;

function TItemKind.GetStringCheckNonEmpty(
  KindsConfig: TKamXMLConfig; const AttrName: string): string;
begin
  Result := KindsConfig.GetValue(VRMLNodeName + '/' + AttrName, '');
  if Result = '' then
    raise Exception.CreateFmt('Empty "%s" attribute for item "%s"',
      [AttrName, VRMLNodeName]);
end;

procedure TItemKind.LoadFromFile(KindsConfig: TKamXMLConfig);
var
  BasePath: string;
begin
  inherited;

  BasePath := ExtractFilePath(KindsConfig.FileName);

  FModelFileName := GetStringCheckNonEmpty(KindsConfig, 'model_file_name');
  FModelFileName := CombinePaths(BasePath, FModelFileName);

  FName := GetStringCheckNonEmpty(KindsConfig, 'name');

  FImageFileName := GetStringCheckNonEmpty(KindsConfig, 'image_file_name');
  FImageFileName := CombinePaths(BasePath, FImageFileName);
end;

function TItemKind.Scene: TVRMLGLScene;
begin
  Result := FScene;
end;

function TItemKind.Image: TImage;
begin
  if FImage = nil then
    FImage := LoadImage(ImageFileName, [], []);
  Result := FImage;
end;

function TItemKind.GLList_DrawImage: TGLuint;
begin
  if FGLList_DrawImage = 0 then
    FGLList_DrawImage := ImageDrawToDisplayList(Image);
  Result := FGLList_DrawImage;
end;

procedure TItemKind.Use(Item: TItem);
begin
  Notification('This item cannot be used');
end;

function TItemKind.BoundingBoxRotated: TBox3D;
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

procedure TItemKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  if FScene = nil then
  begin
    FScene := TVRMLGLScene.CreateCustomCache(nil, GLContextCache);
    FScene.Load(ModelFileName);

    AttributesSet(Scene.Attributes, BlendingType);
    Scene.PrepareResources([prRender, prBoundingBox], false, BaseLights);
    Scene.FreeResources([frTextureDataInNodes]);
  end;

  Progress.Step;
end;

function TItemKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 1;
end;

procedure TItemKind.FreePrepareRender;
begin
  FreeAndNil(FScene);
  inherited;
end;

procedure TItemKind.GLContextClose;
begin
  if FScene <> nil then
    FScene.GLContextClose;
end;

procedure TItemKind.CreateAnimationIfNeeded(
  const AnimationName: string;
  var Anim: TVRMLGLAnimation;
  AnimInfo: TVRMLGLAnimationInfo;
  TransparentGroups: TTransparentGroups;
  const BaseLights: TLightInstancesList);
begin
  inherited CreateAnimationIfNeeded(AnimationName, Anim, AnimInfo,
    TransparentGroups, [prRender, prBoundingBox], BaseLights);
end;

{ TItemKindsList ------------------------------------------------------------- }

procedure TItemKindsList.PrepareRender(const BaseLights: TLightInstancesList);
var
  I: Integer;
  PrepareRenderSteps: Cardinal;
begin
  PrepareRenderSteps := 0;
  for I := 0 to High do
    PrepareRenderSteps += Items[I].PrepareRenderSteps;

  Progress.Init(PrepareRenderSteps, 'Loading items');
  try
    for I := 0 to High do
      Items[I].PrepareRender(BaseLights);
  finally Progress.Fini; end;
end;

procedure TItemKindsList.FreePrepareRender;
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].FreePrepareRender;
end;

procedure TItemKindsList.LoadFromFile;
var
  I: Integer;
  KindsConfig: TKamXMLConfig;
begin
  KindsConfig := TKamXMLConfig.Create(nil);
  try
    KindsConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'kinds.xml';

    for I := 0 to High do
    begin
      Items[I].LoadFromFile(KindsConfig);
    end;
  finally SysUtils.FreeAndNil(KindsConfig); end;
end;

{ TItemPotionOfLifeKind ---------------------------------------------------- }

procedure TItemPotionOfLifeKind.Use(Item: TItem);
begin
  if Player.Life < Player.MaxLife then
  begin
    Player.Life := Min(Player.Life + 50, Player.MaxLife);
    Notification(Format('You drink "%s"', [Item.Kind.Name]));
    Item.Quantity := Item.Quantity - 1;
    SoundEngine.Sound(stPlayerPotionDrink);
  end else
    Notification('You feel quite alright, no need to waste this potion');
end;

{ TItemWeaponKind ------------------------------------------------------------ }

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
    FScreenImage := LoadImage(ScreenImageFileName, [], []);
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
        X := Cardinal(Window.Width) - ScreenImage.Width;
      if ScreenImageAlignBottom then
        Y := 0 else
        Y := Cardinal(Window.Height) - ScreenImage.Height;

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

procedure TItemWeaponKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  inherited;
  CreateAnimationIfNeeded('Attack', FAttackAnimation, FAttackAnimationInfo,
    [tgAll], BaseLights);
end;

function TItemWeaponKind.PrepareRenderSteps: Cardinal;
begin
  Result := (inherited PrepareRenderSteps) + 2;
end;

procedure TItemWeaponKind.FreePrepareRender;
begin
  FreeAndNil(FAttackAnimation);
  inherited;
end;

procedure TItemWeaponKind.GLContextClose;
begin
  inherited;
  if AttackAnimation <> nil then AttackAnimation.GLContextClose;
end;

procedure TItemWeaponKind.LoadFromFile(KindsConfig: TKamXMLConfig);
var
  BasePath: string;
begin
  inherited;

  ActualAttackTime := KindsConfig.GetFloat(VRMLNodeName + '/actual_attack_time',
    DefaultItemActualAttackTime);

  BasePath := ExtractFilePath(KindsConfig.FileName);

  FScreenImageFileName := GetStringCheckNonEmpty(KindsConfig, 'screen_image/file_name');
  FScreenImageFileName := CombinePaths(BasePath, FScreenImageFileName);

  FScreenImageAlignLeft := KindsConfig.GetValue(VRMLNodeName + 'screen_image/align_left', false);
  FScreenImageAlignBottom := KindsConfig.GetValue(VRMLNodeName + 'screen_image/align_bottom', true);

  EquippingSound := SoundEngine.SoundFromName(
    KindsConfig.GetValue(VRMLNodeName + '/equipping_sound', ''));
  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue(VRMLNodeName + '/sound_attack_start', ''));

  { TODO: TItemWeaponKind impl allows to have FAttackAnimationInfo = nil
    if you don't want attack animation, but line below doesn't allow it. }
  AnimationFromConfig(FAttackAnimationInfo, KindsConfig, 'attack');
end;

{ TItemShortRangeWeaponKind -------------------------------------------------- }

constructor TItemShortRangeWeaponKind.Create(const AVRMLNodeName: string);
begin
  inherited;
  FDamageConst := DefaultItemDamageConst;
  FDamageRandom := DefaultItemDamageRandom;
end;

procedure TItemShortRangeWeaponKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  inherited;

  DamageConst := KindsConfig.GetFloat(VRMLNodeName + '/damage/const',
    DefaultItemDamageConst);
  DamageRandom :=KindsConfig.GetFloat(VRMLNodeName + '/damage/random',
    DefaultItemDamageRandom);
end;

{ TItemSwordKind ------------------------------------------------------------- }

procedure TItemSwordKind.ActualAttack(Item: TItem);
var
  WeaponBoundingBox: TBox3D;
  I: Integer;
  C: TCreature;
begin
  WeaponBoundingBox := Box3DTranslate(Player.BoundingBox,
    VectorAdjustToLength(Player.Camera.Direction, 1.0));
  { Tests: Writeln('WeaponBoundingBox is ', Box3DToNiceStr(WeaponBoundingBox)); }
  for I := 0 to Level.Creatures.High do
  begin
    C := Level.Creatures[I];
    { Tests: Writeln('Creature bbox is ', Box3DToNiceStr(C.BoundingBox)); }
    if Boxes3DCollision(C.BoundingBox, WeaponBoundingBox) then
    begin
      C.Life := C.Life - DamageConst - Random * DamageRandom;
      C.LastAttackDirection := Player.Camera.Direction;
    end;
  end;
end;

{ TItemBowKind ------------------------------------------------------------- }

procedure TItemBowKind.ActualAttack(Item: TItem);
var
  QuiverIndex: Integer;
  Missile: TCreature;
  MissilePosition, MissileDirection: TVector3Single;
begin
  QuiverIndex := Player.Items.FindKind(Quiver);
  if QuiverIndex = -1 then
  begin
    Notification('You have no arrows');
    SoundEngine.Sound(stPlayerInteractFailed);
  end else
  begin
    { delete arrow from player }
    Player.Items[QuiverIndex].Quantity :=
      Player.Items[QuiverIndex].Quantity - 1;
    if Player.Items[QuiverIndex].Quantity = 0 then
      Player.DeleteItem(QuiverIndex).Free;

    { shoot the arrow }
    MissilePosition := Player.Camera.Position;
    MissileDirection := Player.Camera.Direction;
    Missile := Arrow.CreateDefaultCreature(MissilePosition, MissileDirection,
      Level.AnimationTime, Arrow.DefaultMaxLife);
    Level.Creatures.Add(Missile);
    SoundEngine.Sound(stArrowFired);
  end;
end;

{ TItemScrollOfFlyingKind ---------------------------------------------------- }

procedure TItemScrollOfFlyingKind.Use(Item: TItem);
begin
  Notification(Format('You cast spell from "%s"', [Item.Kind.Name]));
  Player.FlyingModeTimeoutBegin(30.0);
  Item.Quantity := Item.Quantity - 1;
  SoundEngine.Sound(stPlayerCastFlyingSpell);
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

  Result := Boxes3DCollision(BoundingBox, Player.BoundingBox);
end;

procedure TItemOnLevel.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  if Frustum.Box3DCollisionPossibleSimple(BoundingBox) then
  begin
    glPushMatrix;
      glTranslatev(Position);
      glRotatev(FRotation, UnitVector3Single[2]);
      Item.Kind.Scene.Render(nil, Params);
    glPopMatrix;

    if RenderBoundingBoxes and
       (Params.TransparentGroup in [tgAll, tgOpaque]) then
    begin
      glPushAttrib(GL_ENABLE_BIT);
        glDisable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
        glColorv(Gray3Single);
        glDrawBox3DWire(BoundingBox);
      glPopAttrib;
    end;
  end;
end;

procedure TItemOnLevel.Idle(const CompSpeed: Single);
const
  PositionRadius = 1.0;
  FallingDownSpeed = 0.2;
var
  IsAbove: boolean;
  AboveHeight: Single;
  ShiftedPosition: TVector3Single;
  ProposedNewShiftedPosition, NewShiftedPosition: TVector3Single;
  FallingDownLength: Single;
  AboveGround: PVRMLTriangle;
begin
  FRotation += 3 * CompSpeed * 50;

  ShiftedPosition := Position;
  ShiftedPosition[2] += PositionRadius;

  { Note that I'm using ShiftedPosition, not Position,
    and later I'm comparing "AboveHeight > PositionRadius",
    instead of "AboveHeight > 0".
    Otherwise, I risk that when item will be placed perfectly on the ground,
    it may "slip through" this ground down.

    For the same reason, I use sphere around ShiftedPosition
    when doing Level.MoveAllowed below. }

  Level.GetHeightAbove(ShiftedPosition, IsAbove, AboveHeight, AboveGround);
  if AboveHeight > PositionRadius then
  begin
    { Item falls down because of gravity. }

    FallingDownLength := CompSpeed * 50 * FallingDownSpeed;
    MinTo1st(FallingDownLength, AboveHeight - PositionRadius);

    ProposedNewShiftedPosition := ShiftedPosition;
    ProposedNewShiftedPosition[2] -= FallingDownLength;

    if Level.MoveAllowedSimple(ShiftedPosition, ProposedNewShiftedPosition,
      true, PositionRadius) then
    begin
      FPosition := ProposedNewShiftedPosition;
      FPosition[2] -= PositionRadius;
    end;

    { TODO: I should use Level.MoveAllowed here, not
      Level.MoveAllowedSimple. But then left life potion on gate
      level must be corrected (possibly by correcting the large sword mesh)
      to not "slip down" from the sword. }
    {if Level.MoveAllowed(ShiftedPosition, ProposedNewShiftedPosition,
      NewShiftedPosition, true, PositionRadius) then
    begin
      FPosition := NewShiftedPosition;
      FPosition[2] -= PositionRadius;
    end;}
  end;
end;

function TItemOnLevel.BoundingBox: TBox3D;
begin
  Result := Box3DTranslate(Item.Kind.BoundingBoxRotated, Position);
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
    Notification(S);
  end else
    Notification('You see some item, but it''s too far to tell exactly what it is');
end;

{ TItemsOnLevelList -------------------------------------------------- }

procedure TItemsOnLevelList.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Render(Frustum, Params);
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
  PlayerBoundingBox: TBox3D;
begin
  { Instead of just calling TItemOnLevel.PlayerCollision,
    this is optimized a little: it calculates Player.BoundingBox
    only once. }
  PlayerBoundingBox := Player.BoundingBox;
  for Result := 0 to High do
    if Boxes3DCollision(Items[Result].BoundingBox, PlayerBoundingBox) then
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

procedure GLWindowClose(Window: TGLWindow);
var
  I: Integer;
begin
  { In fact, ItemsKinds will always be nil here, because
    GLWindowClose will be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on ItemsKinds, and this implicitly did GLContextClose,
    so everything is OK. }

  if ItemsKinds <> nil then
  begin
    for I := 0 to ItemsKinds.Count - 1 do
      ItemsKinds[I].GLContextClose;
  end;
end;

procedure DoInitialization;
begin
  Window.OnCloseList.Add(@GLWindowClose);

  ItemsKinds := TItemKindsList.Create;

  Sword := TItemSwordKind.Create('Sword');
  Bow := TItemBowKind.Create('Bow');
  LifePotion := TItemPotionOfLifeKind.Create('LifePotion');
  ScrollOfFlying := TItemScrollOfFlyingKind.Create('ScrFlying');
  KeyItemKind := TItemKind.Create('Key');
  RedKeyItemKind := TItemKind.Create('RedKey');
  Quiver := TItemKind.Create('Arrow');

  ItemsKinds.LoadFromFile;
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