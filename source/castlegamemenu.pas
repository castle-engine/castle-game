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
unit CastleGameMenu;

interface

procedure ShowGameMenu(out AViewAngleChanged: boolean);

implementation

uses SysUtils, Classes, KambiUtils, KambiStringUtils, GLWindow, GLWinModes,
  OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  VectorMath, CastleHelp, CastlePlay, CastleGeneralMenu,
  CastleControlsMenu, CastleKeys, CastleCreatures, CastleChooseMenu,
  CastleItems, GLMenu, RaysWindow;

{ TCastleMenu descendants interface ------------------------------------------ }

type
  TGameMenu = class(TCastleMenu)
    constructor Create;
    procedure CurrentItemSelected; override;
  end;

  TViewAngleSlider = class(TGLMenuFloatSlider)
    constructor Create;
    function ValueToStr(const AValue: Single): string; override;
  end;

  TDebugMenu = class(TCastleMenu)
    ViewAngleSlider: TViewAngleSlider;
    RotationHorizontalSpeedSlider: TGLMenuFloatSlider;
    RotationVerticalSpeedSlider: TGLMenuFloatSlider;
    PlayerSpeedSlider: TGLMenuFloatSlider;
    constructor Create;
    procedure CurrentItemSelected; override;
    procedure CurrentItemAccessoryValueChanged; override;
  end;

{ ----------------------------------------------------------------------------
  global vars (used by TCastleMenu descendants implementation) }

var
  UserQuit: boolean;
  GLList_ScreenImage: TGLuint;
  CurrentMenu: TCastleMenu;
  GameMenu: TGameMenu;
  DebugMenu: TDebugMenu;
  ViewAngleChanged: boolean;

{ TGameMenu ------------------------------------------------------------ }

constructor TGameMenu.Create;
begin
  inherited Create;

  Items.Add('Back to game');
  Items.Add('View last game messages');
  Items.Add('Configure controls');
  Items.Add('End game');
  Items.Add('Debug (cheating) options');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TGameMenu.CurrentItemSelected;
begin
  case CurrentItem of
    0: UserQuit := true;
    1: ViewGameMessages;
    2: ShowControlsMenu(GLList_ScreenImage, true, true);
    3: { At first I did here GameCancel(false), but tests (with Mama)
         show that it's too easy to select this and accidentaly
         end the game. }
       GameCancel(true);
    4: CurrentMenu := DebugMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

{ TViewAngleSlider ----------------------------------------------------------- }

constructor TViewAngleSlider.Create;
begin
  inherited Create(10, 170, ViewAngleDegX);
end;

function TViewAngleSlider.ValueToStr(const AValue: Single): string;
begin
  Result := Format('horiz %f, vert %f', [AValue,
    AdjustViewAngleDegToAspectRatio(AValue, Glw.Height / Glw.Width)]);
end;

{ TDebugMenu ------------------------------------------------------------ }

constructor TDebugMenu.Create;
begin
  inherited Create;

  ViewAngleSlider := TViewAngleSlider.Create;

  { Note that Player is not created at this point.
    We will init Value of these sliders later. }
  RotationHorizontalSpeedSlider := TGLMenuFloatSlider.Create(0.5, 10, 1);
  RotationVerticalSpeedSlider := TGLMenuFloatSlider.Create(0.5, 10, 1);
  PlayerSpeedSlider := TGLMenuFloatSlider.Create(0.1, 5, 1);

  Items.Add('Player.Life := Player.MaxLife');
  Items.Add('Show creatures on level info');
  Items.Add('Add creature to level');
  Items.Add('Change creature kind MoveSpeed');
  Items.Add('Give me 20 instances of every possible item');
  Items.AddObject('Set view angle', ViewAngleSlider);
  Items.AddObject('Set horizontal rotation speed', RotationHorizontalSpeedSlider);
  Items.AddObject('Set vertical rotation speed', RotationVerticalSpeedSlider);
  Items.AddObject('Set player speed', PlayerSpeedSlider);
  Items.Add('Back to main menu');

  FixItemsAreas(Glw.Width, Glw.Height);
end;

procedure TDebugMenu.CurrentItemSelected;

  procedure PlayerMaxLife;
  begin
    if Player.Dead then
      MessageOK(Glw, 'No can do. You are dead.', taLeft) else
    begin
      Player.Life := Player.MaxLife;
      UserQuit := true;
    end;
  end;

  procedure ShowLevelCreaturesInfo;
  var
    I: Integer;
    S: TStringList;
  begin
    S := TStringList.Create;
    try
      S.Append(Format('%d creatures on level:', [Level.Creatures.Count]));
      S.Append('Index: Kind, Position, Life / MaxLife');
      S.Append('');

      for I := 0 to Level.Creatures.High do
        S.Append(Format('%d: %s, %s, %s / %s',
          [ I, Level.Creatures[I].Kind.VRMLNodeName,
            VectorToNiceStr(Level.Creatures[I].LegsPosition),
            FloatToNiceStr(Level.Creatures[I].Life),
            FloatToNiceStr(Level.Creatures[I].MaxLife) ]));

      MessageOK(Glw, S, taLeft);
    finally S.Free end;
  end;

  function ChooseCreatureKind(var ChooseCreature: TCreatureKind): boolean;
  var
    S: TStringList;
    I, ResultIndex: Integer;
  begin
    S := TStringList.Create;
    try
      for I := 0 to CreaturesKinds.High do
        S.Append('Creature ' + CreaturesKinds[I].VRMLNodeName);
      S.Append('Cancel');
      ResultIndex := ChooseByMenu(GLList_ScreenImage, S);
      Result := ResultIndex <> CreaturesKinds.High + 1;
      if Result then
        ChooseCreature := CreaturesKinds[ResultIndex];
    finally S.Free end;
  end;

  procedure AddLevelCreature;
  var
    Position: TVector3Single;
    Direction: TVector3Single;
    Kind: TCreatureKind;
  begin
    if ChooseCreatureKind(Kind) then
    begin
      Position := VectorAdd(Player.Navigator.CameraPos,
        VectorAdjustToLength(Player.Navigator.CameraDir, 10));
      Direction := Player.Navigator.CameraDir;

      Level.Creatures.Add(
        Kind.CreateDefaultCreature(Position, Direction, Level.AnimationTime));

      UserQuit := true;
    end;
  end;

  procedure ChangeCreatureKindMoveSpeed;
  var
    Kind: TCreatureKind;
    MoveSpeed: Single;
  begin
    if ChooseCreatureKind(Kind) then
    begin
      if Kind is TWalkAttackCreatureKind then
        MoveSpeed := (Kind as TWalkAttackCreatureKind).MoveSpeed else
      if Kind is TMissileCreatureKind then
        MoveSpeed := (Kind as TMissileCreatureKind).MoveSpeed else
      begin
        MessageOK(Glw, 'This creature kind has no MoveSpeed property', taLeft);
        Exit;
      end;

      if MessageInputQuerySingle(Glw, 'Input new MoveSpeed for the creature:',
        MoveSpeed, taLeft) then
      begin
        if Kind is TWalkAttackCreatureKind then
          (Kind as TWalkAttackCreatureKind).MoveSpeed := MoveSpeed else
        if Kind is TMissileCreatureKind then
          (Kind as TMissileCreatureKind).MoveSpeed := MoveSpeed else
          raise EInternalError.Create('Kind = ? 2006-04-09');
      end;
    end;
  end;

  procedure GiveItems;
  var
    I: Integer;
  begin
    for I := 0 to ItemsKinds.High do
      Player.PickItem(TItem.Create(ItemsKinds[I], 20));
    UserQuit := true;
  end;

begin
  case CurrentItem of
    0: PlayerMaxLife;
    1: ShowLevelCreaturesInfo;
    2: AddLevelCreature;
    3: ChangeCreatureKindMoveSpeed;
    4: GiveItems;
    5: ;
    6: ;
    7: ;
    8: ;
    9: CurrentMenu := GameMenu;
    else raise EInternalError.Create('Menu item unknown');
  end;
end;

procedure TDebugMenu.CurrentItemAccessoryValueChanged;
begin
  case CurrentItem of
    5: begin
         ViewAngleDegX := ViewAngleSlider.Value;
         ViewAngleChanged := true;
       end;
    6: Player.Navigator.RotationHorizontalSpeed := RotationHorizontalSpeedSlider.Value;
    7: Player.Navigator.RotationVerticalSpeed := RotationVerticalSpeedSlider.Value;
    8: Player.Navigator.CameraDir := VectorAdjustToLength(
         Player.Navigator.CameraDir, PlayerSpeedSlider.Value);
  end;
end;

{ global things -------------------------------------------------------------- }

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(GLList_ScreenImage);

  CurrentMenu.Draw;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  if Key = CastleKey_SaveScreen.Value then
    SaveScreen else
  case C of
    CharEscape: UserQuit := true;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.FpsCompSpeed);
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;

procedure ShowGameMenu(out AViewAngleChanged: boolean);
var
  SavedMode: TGLMode;
begin
  ViewAngleChanged := false;

  DebugMenu.RotationHorizontalSpeedSlider.Value :=
    Player.Navigator.RotationHorizontalSpeed;
  DebugMenu.RotationVerticalSpeedSlider.Value :=
    Player.Navigator.RotationVerticalSpeed;
  DebugMenu.PlayerSpeedSlider.Value :=
    VectorLen(Player.Navigator.CameraDir);

  GLList_ScreenImage := Glw.SaveScreenToDispList;
  try
    SavedMode := TGLMode.Create(Glw, GL_ENABLE_BIT, true);
    try
      SavedMode.FakeMouseDown := false;

      SetStandardGLWindowState(Glw, Draw, CloseQuery, Resize,
        nil, false, true { FPSActive is needed for FpsCompSpeed in Idle. },
        false, K_None, #0, false, false);

      { Otherwise messages don't look good, because the text is mixed
        with the menu text. }
      GLWinMessagesTheme.RectColor[3] := 1.0;

      Glw.OnKeyDown := KeyDown;
      Glw.OnMouseDown := MouseDown;
      Glw.OnMouseUp := MouseUp;
      Glw.OnMouseMove := MouseMove;
      Glw.OnIdle := Idle;

      Glw.EventResize;

      CurrentMenu := GameMenu;
      UserQuit := false;

      glDisable(GL_LIGHTING);

      repeat
        Glwm.ProcessMessage(true);
      until GameEnded or UserQuit;

    finally FreeAndNil(SavedMode); end;
  finally glFreeDisplayList(GLList_ScreenImage); end;

  AViewAngleChanged := ViewAngleChanged;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
begin
  GameMenu := TGameMenu.Create;
  DebugMenu := TDebugMenu.Create;
  CurrentMenu := GameMenu;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CurrentMenu := nil;
  FreeAndNil(GameMenu);
  FreeAndNil(DebugMenu);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.