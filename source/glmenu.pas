unit GLMenu;

interface

uses Classes, OpenGLBmpFonts, BFNT_BitstreamVeraSans_Unit, VectorMath, Areas,
  GLWindow, OpenGLh;

const
  DefaultGLMenuKeyNextItem = K_Down;
  DefaultGLMenuKeyPreviousItem = K_Up;
  DefaultGLMenuKeySelectItem = K_Enter;

  DefaultCurrentItemBorderColor1: TVector3Single = (   1,    1,    1) { White3Single };
  DefaultCurrentItemBorderColor2: TVector3Single = ( 0.5,  0.5,  0.5) { Gray3Single };
  DefaultCurrentItemColor       : TVector3Single = (   1,    1,  0.3) { Yellow3Single };
  DefaultNonCurrentItemColor    : TVector3Single = (   1,    1,    1) { White3Single };

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
    { Position coordinate specifies position of the lower (or left,
      depending whether it's applied to PositionRelativeX or PositionRelativeY)
      border of the menu. }
    prLowerBorder,
    { Position coordinate = 0 means that menu will be in the middle
      of the screen. Other positions will move menu appropriately
      --- higher values to the up (or right), lower to the down (or left). }
    prMiddle,
    { Position coordinate specifies position of the upper (or right)
      border of the menu. }
    prHigherBorder);

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
    MenuAnimation: Single;
    FCurrentItemBorderColor1: TVector3Single;
    FCurrentItemBorderColor2: TVector3Single;
    FCurrentItemColor: TVector3Single;
    FNonCurrentItemColor: TVector3Single;
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
      - KeyDown
      - Idle
      You can call this only while OpenGL context is initialized. }
    procedure FixItemsAreas(const WindowWidth, WindowHeight: Cardinal);

    procedure Draw; virtual;

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

    procedure Idle(const CompSpeed: Single);

    { Called when user will select CurrentItem, either with mouse
      or with keyboard. }
    procedure CurrentItemSelected; virtual;

    { Called when CurrentItem changed, or it's color changed.
      But *not* when CurrentItem changed because of Items.Count changes. }
    procedure CurrentItemChanged; virtual;

    { Default value is DefaultCurrentItemBorderColor1 }
    property CurrentItemBorderColor1: TVector3Single
      read FCurrentItemBorderColor1
      write FCurrentItemBorderColor1;
    { Default value is DefaultCurrentItemBorderColor2 }
    property CurrentItemBorderColor2: TVector3Single
      read FCurrentItemBorderColor2
      write FCurrentItemBorderColor2;
    { Default value is DefaultCurrentItemColor }
    property CurrentItemColor       : TVector3Single
      read FCurrentItemColor write FCurrentItemColor;
    { Default value is DefaultNonCurrentItemColor }
    property NonCurrentItemColor    : TVector3Single
      read FNonCurrentItemColor write FNonCurrentItemColor;
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

  FCurrentItemBorderColor1 := DefaultCurrentItemBorderColor1;
  FCurrentItemBorderColor2 := DefaultCurrentItemBorderColor2;
  FCurrentItemColor := DefaultCurrentItemColor;
  FNonCurrentItemColor := DefaultNonCurrentItemColor;
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
    prLowerBorder: PositionXMove := Position[0];
    prMiddle: PositionXMove :=
      Position[0] + (WindowWidth - AllItemsArea.Width) / 2;
    prHigherBorder: PositionXMove := Position[0] - AllItemsArea.Width;
    else raise EInternalError.Create('PositionRelativeX = ?');
  end;

  case PositionRelativeY of
    prLowerBorder: PositionYMove := Position[1];
    prMiddle: PositionYMove :=
      Position[1] + (WindowHeight - AllItemsArea.Height) / 2;
    prHigherBorder: PositionYMove := Position[1] - AllItemsArea.Height;
    else raise EInternalError.Create('PositionRelativeY = ?');
  end;

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
const
  CurrentItemBorderMargin = 5;
var
  I: Integer;
  CurrentItemBorderColor: TVector3Single;
begin
  glCallList(GLList_DrawFadeRect);

  for I := 0 to Items.Count - 1 do
  begin
    if I = CurrentItem then
    begin
      { Calculate CurrentItemBorderColor }
      if MenuAnimation <= 0.5 then
        CurrentItemBorderColor := VLerp(
          MapRange(MenuAnimation, 0, 0.5, 0, 1),
          CurrentItemBorderColor1, CurrentItemBorderColor2) else
        CurrentItemBorderColor := VLerp(
          MapRange(MenuAnimation, 0.5, 1, 0, 1),
          CurrentItemBorderColor2, CurrentItemBorderColor1);

      glColorv(CurrentItemBorderColor);
      DrawGLRectBorder(
        Areas[I].X0 - CurrentItemBorderMargin,
        Areas[I].Y0,
        Areas[I].X0 + Areas[I].Width + CurrentItemBorderMargin,
        Areas[I].Y0 + Areas[I].Height);

      glColorv(CurrentItemColor);
    end else
      glColorv(NonCurrentItemColor);

    glPushMatrix;
      glTranslatef(Areas[I].X0, Areas[I].Y0 + MenuFont.Descend, 0);
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

procedure TGLMenu.Idle(const CompSpeed: Single);
begin
  MenuAnimation += 0.005 * CompSpeed;
  MenuAnimation := Frac(MenuAnimation);
  CurrentItemChanged;
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