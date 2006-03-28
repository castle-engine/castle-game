unit GLMenu;

interface

uses Classes, OpenGLBmpFonts, BFNT_BitstreamVeraSans_Unit, VectorMath, Areas,
  GLWindow, OpenGLh;

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

  { How TGLMenu.Position will be interpreted. }
  TPositionRelative = (
    { Position (0, 0) means that menu will be in lower-left corner
      of the screen. Other positions will move menu appropriately. }
    prZero,
    { Position (0, 0) means that menu will be in the middle
      of the screen. Other positions will move menu appropriately. }
    prMiddle);

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
    FPositionRelativeX: TPositionRelative;
    FPositionRelativeY: TPositionRelative;
    Areas: TDynAreaArray;
    FKeyNextItem: TKey;
    FKeyPreviousItem: TKey;
    FKeySelectItem: TKey;
    GLList_DrawFadeRect: TGLuint;
    function GetCurrentItem: Integer;
    procedure SetCurrentItem(const Value: Integer);
    { Initializes private as well as global required things. }
    procedure InitGL;
  public
    constructor Create;
    destructor Destroy; override;

    { Position of the lower-left corner of the menu. }
    property Position: TVector2Single read FPosition write FPosition;

    property PositionRelativeX: TPositionRelative
      read FPositionRelativeX write FPositionRelativeX default prMiddle;

    property PositionRelativeY: TPositionRelative
      read FPositionRelativeY write FPositionRelativeY default prMiddle;

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
    procedure FixItemsAreas(const WindowWidth, WindowHeight: Cardinal);

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

uses SysUtils, KambiUtils, KambiGLUtils;

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

  FPositionRelativeX := prMiddle;
  FPositionRelativeY := prMiddle;

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
  glFreeDisplayList(GLList_DrawFadeRect);
end;

procedure TGLMenu.FixItemsAreas(const WindowWidth, WindowHeight: Cardinal);
const
  AllItemsAreaMargin = 30;
var
  I: Integer;
  MaxItemWidth, ItemWidth: Single;
  PositionXMove, PositionYMove: Single;
  AllItemsArea: TArea;
begin
  InitGL;

  { calculate Areas Widths and Heights and MaxItemWidth }

  Areas.Count := 0;
  MaxItemWidth := 0.0;
  for I := 0 to Items.Count - 1 do
  begin
    ItemWidth := MenuFont.TextWidth(Items[I]);
    MaxTo1st(MaxItemWidth, ItemWidth);
    Areas.AppendItem(Area(0, 0, ItemWidth,
      MenuFont.Descend + MenuFont.RowHeight));
  end;

  { calculate AllItemsArea Width and Height }

  AllItemsArea.Width := MaxItemWidth;
  AllItemsArea.Height := (MenuFont.RowHeight + 10) * Areas.Count;

  AllItemsArea.Width += 2 * AllItemsAreaMargin;
  AllItemsArea.Height += 2 * AllItemsAreaMargin;

  { Now take into account Position, PositionRelativeX and PositionRelativeY,
    and calculate PositionXMove, PositionYMove }

  case PositionRelativeX of
    prZero: PositionXMove := 0;
    prMiddle: PositionXMove := (WindowWidth - AllItemsArea.Width) / 2;
    else raise EInternalError.Create('PositionRelativeX = ?');
  end;
  PositionXMove += Position[0];

  case PositionRelativeY of
    prZero: PositionYMove := 0;
    prMiddle: PositionYMove := (WindowHeight - AllItemsArea.Height) / 2;
    else raise EInternalError.Create('PositionRelativeY = ?');
  end;
  PositionYMove += Position[1];

  { Calculate positions of all areas. }

  for I := 0 to Areas.High do
  begin
    Areas[I].X0 := PositionXMove + AllItemsAreaMargin;
    Areas[I].Y0 := PositionYMove + AllItemsAreaMargin
      + (Areas.High - I) * (MenuFont.RowHeight + 10);
  end;
  AllItemsArea.X0 := PositionXMove;
  AllItemsArea.Y0 := PositionYMove;

  { calculate GLList_DrawFadeRect }

  if GLList_DrawFadeRect = 0 then
    GLList_DrawFadeRect := glGenLists(1);
  glNewList(GLList_DrawFadeRect, GL_COMPILE);
  try
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, 0.4);
      glRectf(AllItemsArea.X0, AllItemsArea.Y0,
        AllItemsArea.X0 + AllItemsArea.Width,
        AllItemsArea.Y0 + AllItemsArea.Height);
    glDisable(GL_BLEND);
  finally glEndList end;
end;

procedure TGLMenu.Draw;
var
  I: Integer;
begin
  glCallList(GLList_DrawFadeRect);

  for I := 0 to Items.Count - 1 do
  begin
    glPushMatrix;
      glTranslatef(Areas[I].X0, Areas[I].Y0 + MenuFont.Descend, 0);

      if I = CurrentItem then
      begin
{        glColorv(White3Single);
        DrawGLRectBorder(-10, -MenuFont.Descend,
          MenuFont.TextWidth(Items[I]) + 10, MenuFont.RowHeight);}
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