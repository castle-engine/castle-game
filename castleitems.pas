unit CastleItems;

interface

uses VRMLNodes, VRMLFlatSceneGL, VectorMath, KambiUtils, KambiClassUtils;

{$define read_interface}

type
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
  public
    constructor Create(AModelFileName, AVRMLNodeName: string);
    destructor Destroy; override;

    property ModelFileName: string read FModelFileName;

    { This will be used to refer to item kind from VRML models.

      This should be a valid VRML node name.
      Also, this mustn't contain '_' or '0' ... '9' chars (we may use them
      internally to encode other info in the same VRML node name)
      --- so actually this should only contain letters, 'a'..'z' and 'A'..'Z'.
      Make this in 'CamelCase' to be consistent. }
    property VRMLNodeName: string read FVRMLNodeName;

    { Note that the first call to Scene will try to load model from
      ModelFileName if it wasn't already loaded. }
    function Scene: TVRMLFlatSceneGL;
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
  end;

  TObjectsListItem_1 = TItemOnLevel;
  {$I objectslist_1.inc}
  TItemsOnLevelList = class(TObjectsList_1)
    { Call Render for all items. }
    procedure Render;
    { Call Idle for all items. }
    procedure Idle(const CompSpeed: Single);
  end;

var
  Sword: TItemKind;
  RedFlask: TItemKind;

{ Returns nil if not found. }
function ItemKindWithVRMLNodeName(const VRMLNodeName: string): TItemKind;

{$undef read_interface}

implementation

uses SysUtils, Classes, Object3dAsVRML, GLWindow, CastleWindow,
  OpenGLh, KambiGLUtils;

{$define read_implementation}
{$I objectslist_1.inc}

var
  CreatedItemKinds: TList;

{ TItemKind ------------------------------------------------------------ }

constructor TItemKind.Create(AModelFileName, AVRMLNodeName: string);
begin
  inherited Create;
  FModelFileName := AModelFileName;
  FVRMLNodeName := AVRMLNodeName;
  CreatedItemKinds.Add(Self);
end;

destructor TItemKind.Destroy;
begin
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

{ TItem ------------------------------------------------------------ }

constructor TItem.Create(AKind: TItemKind; AQuantity: Cardinal);
begin
  inherited Create;
  FKind := AKind;
  FQuantity := AQuantity;
  Assert(Quantity >= 1, 'Item''s Quantity must be >= 1');
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
  { TODO }
  Result := false;
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

  Sword := TItemKind.Create('sword.wrl', 'Sword');
  RedFlask := TItemKind.Create('flask_red_processed.wrl', 'FlaskRed');
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