unit CastleItems;

interface

uses Boxes3d, VRMLNodes, VRMLFlatSceneGL, VectorMath, KambiUtils,
  KambiClassUtils, Images, OpenGLh;

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
  TItemKind = class
  private
    FModelFileName: string;
    FScene: TVRMLFlatSceneGL;
    FVRMLNodeName: string;
    FName: string;
    FImageFileName: string;
    FImage: TImage;
    FGLList_DrawImage: TGLuint;
  public
    constructor Create(const AModelFileName, AVRMLNodeName, AName,
      AImageFileName: string);
    destructor Destroy; override;

    property ModelFileName: string read FModelFileName;

    { This will be used to refer to item kind from VRML models.

      This should be a valid VRML node name.
      Also, this mustn't contain '_' or '0' ... '9' chars (we may use them
      internally to encode other info in the same VRML node name)
      --- so actually this should only contain letters, 'a'..'z' and 'A'..'Z'.
      Make this in 'CamelCase' to be consistent. }
    property VRMLNodeName: string read FVRMLNodeName;

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

      Caller of this method should always be prepared to immediately
      handle the "Quantity = 0" situation by freeing given item,
      removing it from any list etc. }
    procedure Use(Item: TItem); virtual;
  end;

  TItemWeaponKind = class(TItemKind)
  private
    FScreenImageFileName: string;
    FScreenImage: TImage;
    FGLList_DrawScreenImage: TGLuint;
    FScreenImageAlignLeft: boolean;
    FScreenImageAlignBottom : boolean;
  public
    constructor Create(const AModelFileName, AVRMLNodeName, AName,
      AImageFileName, AScreenImageFileName: string;
      AScreenImageAlignLeft, AScreenImageAlignBottom: boolean);
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
    { This checks is Item "stackable" with any item on the list.
      Returns index of item on the list that is stackable with given Item,
      or -1 if none. }
    function Stackable(Item: TItem): Integer;
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
      2. doesn't modify current matrix). }
    procedure Render;

    procedure Idle(const CompSpeed: Single);

    { This is Item.Kind.Scene.BoundingBox translated by our current Position. }
    function BoundingBox: TBox3d;
  end;

  TObjectsListItem_1 = TItemOnLevel;
  {$I objectslist_1.inc}
  TItemsOnLevelList = class(TObjectsList_1)
    { Call Render for all items. }
    procedure Render;
    { Call Idle for all items. }
    procedure Idle(const CompSpeed: Single);
    { Check collision with all items, returns index of first collider
      (or -1 if no collision). }
    function PlayerCollision: Integer;
  end;

var
  Sword: TItemKind;
  LifePotion: TItemKind;

{ Returns nil if not found. }
function ItemKindWithVRMLNodeName(const VRMLNodeName: string): TItemKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, Object3dAsVRML, GLWindow, CastleWindow,
  KambiGLUtils, CastlePlay;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

var
  CreatedItemKinds: TList;

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(const AModelFileName, AVRMLNodeName, AName,
  AImageFileName: string);
begin
  inherited Create;
  FModelFileName := AModelFileName;
  FVRMLNodeName := AVRMLNodeName;
  FName := AName;
  FImageFileName := AImageFileName;
  CreatedItemKinds.Add(Self);
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
  GameMessage('This item cannot be used');
end;

{ TItemWeaponKind ------------------------------------------------------------ }

constructor TItemWeaponKind.Create(const AModelFileName, AVRMLNodeName, AName,
  AImageFileName, AScreenImageFileName: string;
  AScreenImageAlignLeft, AScreenImageAlignBottom: boolean);
begin
  inherited Create(AModelFileName, AVRMLNodeName, AName,
    AImageFileName);
  FScreenImageFileName := AScreenImageFileName;
  FScreenImageAlignLeft := AScreenImageAlignLeft;
  FScreenImageAlignBottom := AScreenImageAlignBottom;
end;

destructor TItemWeaponKind.Destroy;
begin
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
    FGLList_DrawScreenImage := glGenLists(1);
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
    This works quite good, and is "precise enough" :) }

  Result := Boxes3dCollision(BoundingBox, Player.BoundingBox);
end;

procedure TItemOnLevel.Render;
begin
  glPushMatrix;
    glTranslatev(Position);
    glRotatev(FRotation, UnitVector3Single[2]);
    Item.Kind.Scene.Render(nil);
  glPopMatrix;
end;

procedure TItemOnLevel.Idle(const CompSpeed: Single);
begin
  FRotation += 3 * CompSpeed;
end;

function TItemOnLevel.BoundingBox: TBox3d;
begin
  Result := Box3dTranslate(Item.Kind.Scene.BoundingBox, Position);
end;

{ TItemsOnLevelList -------------------------------------------------- }

procedure TItemsOnLevelList.Render;
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Render;
end;

procedure TItemsOnLevelList.Idle(const CompSpeed: Single);
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].Idle(CompSpeed);
end;

function TItemsOnLevelList.PlayerCollision: Integer;
begin
  { TODO: optimize this, to not calculate Player.BoundingBox for each item. }
  for Result := 0 to High do
    if Items[Result].PlayerCollision then
      Exit;
  Result := -1;
end;

{ other global stuff --------------------------------------------------- }

function ItemKindWithVRMLNodeName(const VRMLNodeName: string): TItemKind;
var
  I: Integer;
begin
  for I := 0 to CreatedItemKinds.Count - 1 do
  begin
    Result := TItemKind(CreatedItemKinds.Items[I]);
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
  { In fact, CreatedItemKinds will always be nil here, because
    GLWindowClose will be called from CastleWindow unit finalization
    that will be done after this unit's finalization (DoFinalization).

    That's OK --- DoFinalization already freed
    every item on CreatedItemKinds.Items, and this implicitly did CloseGL,
    so everything is OK. }

  if CreatedItemKinds <> nil then
  begin
    for I := 0 to CreatedItemKinds.Count - 1 do
      if TItemKind(CreatedItemKinds.Items[I]).FScene <> nil then
        TItemKind(CreatedItemKinds.Items[I]).FScene.CloseGL;
  end;
end;

procedure DoInitialization;
begin
  Glw.OnCloseList.AppendItem(@GLWindowClose);

  CreatedItemKinds := TList.Create;

  Sword := TItemWeaponKind.Create('sword.wrl', 'Sword', 'Sword', 'sword.png',
    'sword.png', false, true);

  LifePotion := TItemKind.Create('flask_red_processed.wrl', 'LifePotion',
    'Potion of Life', 'flask_red.png');
end;

procedure DoFinalization;
var
  I: Integer;
begin
  for I := 0 to CreatedItemKinds.Count - 1 do
    TItemKind(CreatedItemKinds.Items[I]).Free;
  FreeAndNil(CreatedItemKinds);
end;

initialization
  DoInitialization;
finalization
  DoFinalization;
end.