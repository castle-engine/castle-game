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

{ }
unit GameItems;

interface

uses Boxes3D, X3DNodes, CastleScene, VectorMath, CastleUtils,
  CastleClassUtils, Classes, Images, GL, GLU, CastleGLUtils, GameSound,
  PrecalculatedAnimation, GameObjectKinds,
  CastleXMLConfig, XmlSoundEngine, Frustum, Base3D,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif}, CastleColors;

const
  DefaultItemDamageConst = 5.0;
  DefaultItemDamageRandom = 5.0;
  DefaultItemActualAttackTime = 0.0;

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
    FScene: TCastleScene;
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
      var Anim: TCastlePrecalculatedAnimation;
      AnimInfo: string;
      const BaseLights: TLightInstancesList);

    function GetStringCheckNonEmpty(KindsConfig: TCastleConfig;
      const AttrName: string): string;
  public
    { The constructor. }
    constructor Create(const AShortName: string);
    destructor Destroy; override;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;

    property ModelFileName: string read FModelFileName;

    { Nice name for user. }
    property Name: string read FName;

    { Note that the Scene is nil if not PrepareRenderDone. }
    function Scene: TCastleScene;

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

  TItemKindList = class(specialize TFPGObjectList<TItemKind>)
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
    FAttackAnimation: TCastlePrecalculatedAnimation;
    FAttackAnimationFile: string;
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
    property AttackAnimation: TCastlePrecalculatedAnimation
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

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
  end;

  TItemShortRangeWeaponKind = class(TItemWeaponKind)
  private
    FDamageConst: Single;
    FDamageRandom: Single;
  public
    constructor Create(const AShortName: string);

    property DamageConst: Single read FDamageConst write FDamageConst
      default DefaultItemDamageConst;
    property DamageRandom: Single read FDamageRandom write FDamageRandom
      default DefaultItemDamageRandom;

    procedure LoadFromFile(KindsConfig: TCastleConfig); override;
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

  TItemList = class(specialize TFPGObjectList<TItem>)
  public
    { This checks is Item "stackable" with any item on the list.
      Returns index of item on the list that is stackable with given Item,
      or -1 if none. }
    function Stackable(Item: TItem): Integer;

    { Searches for item of given Kind. Returns index of first found,
      or -1 if not found. }
    function FindKind(Kind: TItemKind): Integer;
  end;

  TItemOnLevel = class(T3DTransform)
  private
    FItem: TItem;
  protected
    function GetExists: boolean; override;
  public
    constructor Create(AOwner: TComponent;
      AItem: TItem; const ATranslation: TVector3Single); reintroduce;
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

    { Render the item, on current Position with current rotation etc.
      Current matrix should be modelview, this pushes/pops matrix state
      (so it 1. needs one place on matrix stack,
      2. doesn't modify current matrix).

      Pass current viewing Frustum to allow optimizing this
      (when item for sure is not within Frustum, we don't have
      to push it to OpenGL). }
    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;

    function PointingDeviceActivate(const Active: boolean;
      const Distance: Single): boolean; override;

    property Collides default false;
    property Pushable default true;
  end;

  TItemOnLevelList = class(specialize TFPGObjectList<TItemOnLevel>)
  end;

var
  ItemsKinds: TItemKindList;

  Sword: TItemSwordKind;
  Bow: TItemBowKind;
  LifePotion: TItemKind;
  ScrollOfFlying: TItemKind;
  KeyItemKind: TItemKind;
  RedKeyItemKind: TItemKind;
  Quiver: TItemKind;

const
  DefaultAutoOpenInventory = true;

var
  { Automatically open inventory on pickup ?
    Saved/loaded to config file in this unit. }
  AutoOpenInventory: boolean;

  InventoryVisible: boolean;

{ Returns nil if not found. }
function ItemKindWithShortName(const ShortName: string): TItemKind;

implementation

uses SysUtils, CastleWindow, GameWindow,
  GamePlay, CastleFilesUtils, ProgressUnit,
  GameCreatures, GameVideoOptions, GameNotifications, GameConfig,
  CastleSceneCore, Triangle, GLImages;

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(const AShortName: string);
begin
  inherited Create(AShortName);
  ItemsKinds.Add(Self);
end;

destructor TItemKind.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FScene);
  inherited;
end;

function TItemKind.GetStringCheckNonEmpty(
  KindsConfig: TCastleConfig; const AttrName: string): string;
begin
  Result := KindsConfig.GetValue(ShortName + '/' + AttrName, '');
  if Result = '' then
    raise Exception.CreateFmt('Empty "%s" attribute for item "%s"',
      [AttrName, ShortName]);
end;

procedure TItemKind.LoadFromFile(KindsConfig: TCastleConfig);
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

function TItemKind.Scene: TCastleScene;
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
  Notifications.Show('This item cannot be used');
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
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[0, 0], FBoundingBoxRotated.Data[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[1, 0], FBoundingBoxRotated.Data[0, 1])),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[1, 0], FBoundingBoxRotated.Data[1, 1]))),
      VectorLenSqr(Vector2Single(FBoundingBoxRotated.Data[0, 0], FBoundingBoxRotated.Data[1, 1])));
    HorizontalSize := Sqrt(HorizontalSize);
    FBoundingBoxRotated.Data[0, 0] := -HorizontalSize;
    FBoundingBoxRotated.Data[0, 1] := -HorizontalSize;
    FBoundingBoxRotated.Data[1, 0] := +HorizontalSize;
    FBoundingBoxRotated.Data[1, 1] := +HorizontalSize;

    FBoundingBoxRotatedCalculated := true;
  end;
  Result := FBoundingBoxRotated;
end;

procedure TItemKind.PrepareRenderInternal(const BaseLights: TLightInstancesList);
begin
  if FScene = nil then
  begin
    FScene := TCastleScene.CreateCustomCache(nil, GLContextCache);
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
  var Anim: TCastlePrecalculatedAnimation;
  AnimInfo: string;
  const BaseLights: TLightInstancesList);
begin
  inherited CreateAnimationIfNeeded(AnimationName, Anim, AnimInfo,
    [prRender, prBoundingBox], BaseLights);
end;

{ TItemKindList ------------------------------------------------------------- }

procedure TItemKindList.PrepareRender(const BaseLights: TLightInstancesList);
var
  I: Integer;
  PrepareRenderSteps: Cardinal;
begin
  PrepareRenderSteps := 0;
  for I := 0 to Count - 1 do
    PrepareRenderSteps += Items[I].PrepareRenderSteps;

  Progress.Init(PrepareRenderSteps, 'Loading items');
  try
    for I := 0 to Count - 1 do
      Items[I].PrepareRender(BaseLights);
  finally Progress.Fini; end;
end;

procedure TItemKindList.FreePrepareRender;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].FreePrepareRender;
end;

procedure TItemKindList.LoadFromFile;
var
  I: Integer;
  KindsConfig: TCastleConfig;
begin
  KindsConfig := TCastleConfig.Create(nil);
  try
    KindsConfig.FileName := ProgramDataPath + 'data' + PathDelim +
      'items' + PathDelim + 'kinds.xml';

    for I := 0 to Count - 1 do
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
    Notifications.Show(Format('You drink "%s"', [Item.Kind.Name]));
    Item.Quantity := Item.Quantity - 1;
    SoundEngine.Sound(stPlayerPotionDrink);
  end else
    Notifications.Show('You feel quite alright, no need to waste this potion');
end;

{ TItemWeaponKind ------------------------------------------------------------ }

destructor TItemWeaponKind.Destroy;
begin
  FreeAndNil(FAttackAnimation);
  FAttackAnimationFile := '';
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
  CreateAnimationIfNeeded('Attack', FAttackAnimation, FAttackAnimationFile,
    BaseLights);
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

procedure TItemWeaponKind.LoadFromFile(KindsConfig: TCastleConfig);
var
  BasePath: string;
begin
  inherited;

  ActualAttackTime := KindsConfig.GetFloat(ShortName + '/actual_attack_time',
    DefaultItemActualAttackTime);

  BasePath := ExtractFilePath(KindsConfig.FileName);

  FScreenImageFileName := GetStringCheckNonEmpty(KindsConfig, 'screen_image/file_name');
  FScreenImageFileName := CombinePaths(BasePath, FScreenImageFileName);

  FScreenImageAlignLeft := KindsConfig.GetValue(ShortName + 'screen_image/align_left', false);
  FScreenImageAlignBottom := KindsConfig.GetValue(ShortName + 'screen_image/align_bottom', true);

  EquippingSound := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/equipping_sound', ''));
  SoundAttackStart := SoundEngine.SoundFromName(
    KindsConfig.GetValue(ShortName + '/sound_attack_start', ''));

  { TODO: TItemWeaponKind impl allows to have FAttackAnimationFile = ''
    if you don't want attack animation, but line below doesn't allow it. }
  AnimationFromConfig(FAttackAnimationFile, KindsConfig, 'attack');
end;

{ TItemShortRangeWeaponKind -------------------------------------------------- }

constructor TItemShortRangeWeaponKind.Create(const AShortName: string);
begin
  inherited;
  FDamageConst := DefaultItemDamageConst;
  FDamageRandom := DefaultItemDamageRandom;
end;

procedure TItemShortRangeWeaponKind.LoadFromFile(KindsConfig: TCastleConfig);
begin
  inherited;

  DamageConst := KindsConfig.GetFloat(ShortName + '/damage/const',
    DefaultItemDamageConst);
  DamageRandom :=KindsConfig.GetFloat(ShortName + '/damage/random',
    DefaultItemDamageRandom);
end;

{ TItemSwordKind ------------------------------------------------------------- }

procedure TItemSwordKind.ActualAttack(Item: TItem);
var
  WeaponBoundingBox: TBox3D;
  I: Integer;
  C: TCreature;
begin
  WeaponBoundingBox := Player.BoundingBox.Translate(
    VectorAdjustToLength(Player.Camera.Direction, 1.0));
  { Tests: Writeln('WeaponBoundingBox is ', WeaponBoundingBox.ToNiceStr); }
  { TODO: we would prefer to use Level.Items.BoxCollision for this,
    but we need to know which creature was hit. }
  for I := 0 to Level.Items.Count - 1 do
    if Level.Items[I] is TCreature then
    begin
      C := TCreature(Level.Items[I]);
      { Tests: Writeln('Creature bbox is ', C.BoundingBox.ToNiceStr); }
      if C.BoundingBox.Collision(WeaponBoundingBox) then
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
begin
  QuiverIndex := Player.Items.FindKind(Quiver);
  if QuiverIndex = -1 then
  begin
    Notifications.Show('You have no arrows');
    SoundEngine.Sound(stPlayerInteractFailed);
  end else
  begin
    { delete arrow from player }
    Player.Items[QuiverIndex].Quantity :=
      Player.Items[QuiverIndex].Quantity - 1;
    if Player.Items[QuiverIndex].Quantity = 0 then
      Player.DeleteItem(QuiverIndex).Free;

    { shoot the arrow }
    Level.CreateCreature(Arrow, Player.Camera.Position, Player.Camera.Direction);
    SoundEngine.Sound(stArrowFired);
  end;
end;

{ TItemScrollOfFlyingKind ---------------------------------------------------- }

procedure TItemScrollOfFlyingKind.Use(Item: TItem);
begin
  Notifications.Show(Format('You cast spell from "%s"', [Item.Kind.Name]));
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

{ TItemList ------------------------------------------------------------ }

function TItemList.Stackable(Item: TItem): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Stackable(Item) then
      Exit;
  Result := -1;
end;

function TItemList.FindKind(Kind: TItemKind): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Kind = Kind then
      Exit;
  Result := -1;
end;

{ TItemOnLevel ------------------------------------------------------------ }

constructor TItemOnLevel.Create(AOwner: TComponent;
  AItem: TItem; const ATranslation: TVector3Single);
begin
  inherited Create(AOwner);
  FItem := AItem;
  Translation := ATranslation;
  Rotation := Vector4Single(UnitVector3Single[2], 0); { angle will animate later }

  { most item models are not 2-manifold }
  CastShadowVolumes := false;

  Pushable := true;

  Add(Item.Kind.Scene);

  { Items are not collidable, player can enter them to pick them up.
    For now, this also means that creatures can pass through them,
    which isn't really troublesome now. }
  Collides := false;
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

procedure TItemOnLevel.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  inherited;
  if GetExists and RenderBoundingBoxes and
    (not Params.Transparent) and Params.ShadowVolumesReceivers and
    Frustum.Box3DCollisionPossibleSimple(BoundingBox) then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glEnable(GL_DEPTH_TEST);
      glColorv(Gray3Single);
      glDrawBox3DWire(BoundingBox);
    glPopAttrib;
  end;
end;

procedure TItemOnLevel.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
const
  Radius = 1.0;
  FallingDownSpeed = 0.2;
var
  IsAbove: boolean;
  AboveHeight: Single;
  ShiftedTranslation: TVector3Single;
  NewTranslation: TVector3Single;
  FallingDownLength: Single;
  AboveGround: PTriangle;
  Rot: TVector4Single;
begin
  inherited;
  if not GetExists then Exit;

  Rot := Rotation;
  Rot[3] += 2.61 * CompSpeed;
  Rotation := Rot;

  ShiftedTranslation := Translation;
  ShiftedTranslation[2] += Radius;

  { Note that I'm using ShiftedTranslation, not Translation,
    and later I'm comparing "AboveHeight > Radius",
    instead of "AboveHeight > 0".
    Otherwise, I risk that when item will be placed perfectly on the ground,
    it may "slip through" this ground down.

    For the same reason, I use sphere around ShiftedTranslation
    when doing Level.MoveAllowed below. }

  Level.GetHeightAbove(ShiftedTranslation, IsAbove, AboveHeight, AboveGround);
  if AboveHeight > Radius then
  begin
    { Item falls down because of gravity. }

    FallingDownLength := CompSpeed * 50 * FallingDownSpeed;
    MinTo1st(FallingDownLength, AboveHeight - Radius);

    NewTranslation := ShiftedTranslation;
    NewTranslation[2] -= FallingDownLength;

    { TODO: I could use Level.MoveAllowed with wall-sliding here.
      But then left life potion on gate
      level must be corrected (possibly by correcting the large sword mesh)
      to not "slip down" from the sword. }
    {if Level.MoveAllowed(ShiftedTranslation, NewTranslation,
      RealNewTranslation, true, Radius) then}
    { TODO: just use item box here, instead of (invalid) radius? }
    if Level.MoveAllowed(ShiftedTranslation, NewTranslation, true, Radius,
      Box3DAroundPoint(ShiftedTranslation, Radius * 2),
      Box3DAroundPoint(NewTranslation    , Radius * 2)) then
    begin
      NewTranslation[2] -= Radius;
      Translation := NewTranslation;
    end;
  end;

  if (not Player.Dead) and (not GameWin) and
    BoundingBox.Collision(Player.BoundingBox) then
  begin
    Player.PickItem(ExtractItem);
    RemoveMe := rtRemoveAndFree;
    if AutoOpenInventory then
      InventoryVisible := true;
  end;
end;

function TItemOnLevel.PointingDeviceActivate(const Active: boolean;
  const Distance: Single): boolean;
const
  VisibleItemDistance = 60.0;
var
  S: string;
begin
  Result := Active;
  if not Result then Exit;

  if Distance <= VisibleItemDistance then
  begin
    S := Format('You see an item "%s"', [Item.Kind.Name]);
    if Item.Quantity <> 1 then
      S += Format(' (quantity %d)', [Item.Quantity]);
    Notifications.Show(S);
  end else
    Notifications.Show('You see some item, but it''s too far to tell exactly what it is');
end;

function TItemOnLevel.GetExists: boolean;
begin
  Result := (inherited GetExists) and (not DebugRenderForLevelScreenshot);
end;

{ other global stuff --------------------------------------------------- }

function ItemKindWithShortName(const ShortName: string): TItemKind;
var
  I: Integer;
begin
  for I := 0 to ItemsKinds.Count - 1 do
  begin
    Result := ItemsKinds.Items[I];
    if Result.ShortName = ShortName then
      Exit;
  end;
  Result := nil;
end;

{ initialization / finalization ---------------------------------------- }

procedure WindowClose(Window: TCastleWindowBase);
var
  I: Integer;
begin
  { In fact, ItemsKinds will always be nil here, because
    WindowClose will be called from CastleWindow unit finalization
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
  Window.OnCloseList.Add(@WindowClose);

  ItemsKinds := TItemKindList.Create(true);

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
  FreeAndNil(ItemsKinds);
end;

initialization
  DoInitialization;
  AutoOpenInventory := ConfigFile.GetValue(
    'auto_open_inventory', DefaultAutoOpenInventory);
finalization
  DoFinalization;
  ConfigFile.SetDeleteValue('auto_open_inventory',
    AutoOpenInventory, DefaultAutoOpenInventory);
end.