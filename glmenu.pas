unit GLMenu;

interface

uses Classes, OpenGLBmpFonts, BFNT_BitstreamVeraSans_Unit, VectorMath, Areas,
  GLWindow;

const
  DefaultGLMenuKeyNextItem = K_Down;
  DefaultGLMenuKeyPreviousItem = K_Up;
  DefaultGLMenuKeySelectItem = K_Enter;

type
  { This is something that can be attached to some menu items of TGLMenu.
    For example, a slider --- see TGLMenuSlider. }
  TGLMenuItemAccessory = class
    procedure Draw; virtual; abstract;
  end;

  TGLMenuSlider = class(TGLMenuItemAccessory)
  end;

  TGLMenuFloatSlider = class(TGLMenuSlider)
  private
    FBeginRange: Single;
    FEndRange: Single;
    FValue: Single;
  public
    constructor Create(const ABeginRange, AEndRange, AValue: Single);
    property BeginRange: Single read FBeginRange;
    property EndRange: Single read FEndRange;
    property Value: Single read FValue;
    procedure Draw; override;
  end;

  { A menu displayed in OpenGL.

    Note that all 2d positions and sizes for this class are interpreted
    as pixel positions on your 2d screen (for glRaster, glBitmap etc.)
    and also as normal positions (for glTranslatef etc.) on your 2d screen.
    Smaller x positions are considered more to the left,
    smaller y positions are considered lower.
    Stating it simpler: just make sure that your OpenGL projection is
    @code(ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);) }
  TGLMenu = class
  private
    FItems: TStringList;
    FCurrentItem: Integer;
    FPosition: TVector2Single;
    Areas: TDynAreaArray;
    FKeyNextItem: TKey;
    FKeyPreviousItem: TKey;
    FKeySelectItem: TKey;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    { Initializes private as well as global required things. }
    procedure InitGL;
  public
    constructor Create;
    destructor Destroy; override;

    property Position: TVector2Single read FPosition write FPosition;

    property Items: TStringList read FItems;

    { When Items.Count <> 0, this is always some number
      between 0 and Items.Count - 1.
      Otherwise (when Items.Count <> 0) this is always -1.

      If you assign it to wrong value (breaking conditions above),
      or if you change Items such that conditions are broken,
      it will be arbitrarily fixed.

      Changing this calls CurrentItemChanged automatically when needed. }
    property CurrentItem: Integer read GetCurrentItem write SetCurrentItem;

    { These change CurrentItem as appropriate.
      Usually you will just let this class call it internally
      (from MouseMove, KeyDown etc.) and will not need to call it yourself. }
    procedure NextItem;
    procedure PreviousItem;

    { Release things associated with OpenGL context.
      This will be also automatically called from destructor. }
    procedure CloseGL;

    { You must call FixItemsAreas between last modification of
      - Items and/or
      - Position
      and calling procedures
      - Draw or
      - MouseMove or
      - MouseDown
      You can call this only while OpenGL context is initialized. }
    procedure FixItemsAreas;

    procedure Draw;

    property KeyNextItem: TKey read FKeyNextItem write FKeyNextItem
      default DefaultGLMenuKeyNextItem;
    property KeyPreviousItem: TKey read FKeyPreviousItem write FKeyPreviousItem
      default DefaultGLMenuKeyPreviousItem;
    property KeySelectItem: TKey read FKeySelectItem write FKeySelectItem
      default DefaultGLMenuKeySelectItem;

    procedure KeyDown(Key: TKey; C: char);

    { Call this when user moves the mouse.
      NewX, NewY is in OpenGL 2d screen coordinates, so usually
      (when you call this from TGLWindow.OnMouseMove) you will
      have to flip the NewY like @code(Glwin.Height - NewY). }
    procedure MouseMove(const NewX, NewY: Single);

    procedure MouseDown(const MouseX, MouseY: Single; Button: TMouseButton);

    { Called when user will select CurrentItem, either with mouse
      or with keyboard. }
    procedure CurrentItemSelected; virtual;

    { Called when CurrentItem changed, but *not* when CurrentItem
      changed because of Items.Count changes. }
    procedure CurrentItemChanged; virtual;
  end;

var
  { It will be automatically initialized by any TGLMenu operation
    that requires it. You can set this yourself or just let TGLMenu
    to set it.

    YOU MUST RELEASE IT YOURSELF. Don't forget about it. }
  MenuFont: TGLBitmapFont;

implementation

uses SysUtils, OpenGLh, KambiUtils, KambiGLUtils;

{ TGLMenuFloatSlider --------------------------------------------------------- }

constructor TGLMenuFloatSlider.Create(
  const ABeginRange, AEndRange, AValue: Single);
begin
  inherited Create;
  FBeginRange := ABeginRange;
  FEndRange := AEndRange;
  FValue := AValue;
end;

procedure TGLMenuFloatSlider.Draw;
begin
  { TODO }
end;

{ TGLMenu -------------------------------------------------------------------- }

constructor TGLMenu.Create;
begin
  inherited;
  FItems := TStringList.Create;
  FCurrentItem := 0;
  Areas := TDynAreaArray.Create;

  KeyNextItem := DefaultGLMenuKeyNextItem;
  KeyPreviousItem := DefaultGLMenuKeyPreviousItem;
  KeySelectItem := DefaultGLMenuKeySelectItem;
end;

destructor TGLMenu.Destroy;
begin
  CloseGL;
  FreeAndNil(FItems);
  FreeAndNil(Areas);
  inherited;
end;

function TGLMenu.GetCurrentItem: Integer;
begin
  Result := FCurrentItem;

  { Make sure that CurrentItem conditions are OK.

    Alternatively we could watch for this in SetCurrentItem, but then
    changing Items by user of this class could invalidate it.
    So it's safest to just check the conditions here. }

  if Items.Count <> 0 then
  begin
    Clamp(Result, 0, Items.Count - 1);
  end else
    Result := -1;
end;

procedure TGLMenu.SetCurrentItem(const Value: Integer);
var
  OldCurrentItem, NewCurrentItem: Integer;
begin
  OldCurrentItem := CurrentItem;
  FCurrentItem := Value;
  NewCurrentItem := CurrentItem;
  if OldCurrentItem <> NewCurrentItem then
    CurrentItemChanged;
end;

procedure TGLMenu.NextItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = Items.Count - 1 then
      CurrentItem := 0 else
      CurrentItem := CurrentItem + 1;
  end;
end;

procedure TGLMenu.PreviousItem;
begin
  if Items.Count <> 0 then
  begin
    if CurrentItem = 0 then
      CurrentItem := Items.Count - 1 else
      CurrentItem := CurrentItem - 1;
  end;
end;

procedure TGLMenu.InitGL;
begin
  if MenuFont = nil then
    MenuFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure TGLMenu.CloseGL;
begin
  { Nothing to do here right now. }
end;

procedure TGLMenu.FixItemsAreas;
var
  I: Integer;
begin
  InitGL;

  Areas.Count := 0;
  for I := 0 to Items.Count - 1 do
  begin
    Areas.AppendItem(Area(
      Position[0],
      Position[1] - Ord(I) * (MenuFont.RowHeight + 10) - MenuFont.Descend,
      MenuFont.TextWidth(Items[I]),
      MenuFont.Descend + MenuFont.RowHeight, 0));
  end;
end;

procedure TGLMenu.Draw;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    glPushMatrix;
      glTranslatef(Position[0],
        Position[1] - Ord(I) * (MenuFont.RowHeight + 10), 0);

      if I = CurrentItem then
      begin
        glColorv(White3Single);
        DrawGLRectBorder(-10, -MenuFont.Descend,
          MenuFont.TextWidth(Items[I]) + 10, MenuFont.RowHeight);
        glColorv(Yellow3Single);
      end else
        glColorv(White3Single);

      glRasterPos2i(0, 0);
      MenuFont.Print(Items[I]);
    glPopMatrix;
  end;
end;

procedure TGLMenu.KeyDown(Key: TKey; C: char);
begin
  if Key = KeyPreviousItem then
    PreviousItem else
  if Key = KeyNextItem then
    NextItem else
  if Key = KeySelectItem then
    CurrentItemSelected;
end;

procedure TGLMenu.MouseMove(const NewX, NewY: Single);
var
  NewItemIndex: Integer;
begin
  NewItemIndex := Areas.FindArea(NewX, NewY);
  if NewItemIndex <> -1 then
    CurrentItem := NewItemIndex;
end;

procedure TGLMenu.MouseDown(const MouseX, MouseY: Single; Button: TMouseButton);
var
  NewItemIndex: Integer;
begin
  if Button = mbLeft then
  begin
    NewItemIndex := Areas.FindArea(MouseX, MouseY);
    if NewItemIndex <> -1 then
    begin
      CurrentItem := NewItemIndex;
      CurrentItemSelected;
    end;
  end;
end;

procedure TGLMenu.CurrentItemSelected;
begin
  { Nothing to do in this class. }
end;

procedure TGLMenu.CurrentItemChanged;
begin
  { Nothing to do in this class. }
end;

end.