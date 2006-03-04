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

unit CastleMenu;

interface

{ Show menu, ask user what to do, do what the user wants
  (e.g. load level and call PlayLevel), when user wants to quit -- return. }
procedure ShowMenu;

implementation

uses SysUtils, KambiUtils, GLWindow, GLWinModes,
  OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  OpenGLBmpFonts, VectorMath, Images, BFNT_BitstreamVeraSans_Unit,
  CastleLevel, CastlePlay, CastleSound, CastlePlayer, CastleHelp;

type
  TMenuItem = (miReadDocs, miPlaySample1, miPlaySample2,
    miViewGameMessages, miSound, miQuit);

const
  MenuNames: array[TMenuItem]of string = (
    'Read short instructions',
    'New game (Sample castle level)',
    'New game (Castle Hall - new level for PGD stage 3)',
    'View last game messages',
    'Sound',
    'Quit');

var
  UserQuit: boolean;
  CurrentMenu: TMenuItem = Low(TMenuItem);
  ListBgDraw: TGLuint;
  MenuFont: TGLBitmapFont;

procedure Resize(Glwin: TGLWindow);
begin
  ProjectionGLOrtho(0, Glwin.Width, 0, Glwin.Height);
end;

procedure Draw(Glwin: TGLWindow);
var
  MI: TMenuItem;
begin
  glLoadIdentity;
  glRasterPos2i(0, 0);
  glCallList(ListBgDraw);

  glTranslatef(glw.width*50 div 640, glw.height*350 div 480, 0);
  for MI := Low(MI) to High(MI) do
  begin
    glPushMatrix;
      glTranslatef(0, -Ord(MI) * (MenuFont.RowHeight + 10), 0);

      if MI = CurrentMenu then
      begin
        glColorv(White3Single);
        DrawGLRectBorder(-10, -Menufont.Descend,
          MenuFont.TextWidth(MenuNames[MI])+10, MenuFont.RowHeight);
        glColorv(Yellow3Single);
      end else
        glColorv(White3Single);

      glRasterPos2i(0, 0);
      MenuFont.Print(MenuNames[MI]);
    glPopMatrix;
  end;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);

  procedure NewGame(Level: TLevel);
  var
    Player: TPlayer;
  begin
    try
      GameMessages.Clear;
      Player := TPlayer.Create;
      try
        PlayLevel(Level, Player);
      finally Player.Free end;
    finally Level.Free end;
  end;

begin
  case key of
    K_Up:
      begin
        if CurrentMenu = Low(CurrentMenu) then
          CurrentMenu := High(CurrentMenu) else
          CurrentMenu := Pred(CurrentMenu);
        Glw.PostRedisplay;
      end;
    K_Down:
      begin
        if CurrentMenu = High(CurrentMenu) then
          CurrentMenu := Low(CurrentMenu) else
          CurrentMenu := Succ(CurrentMenu);
        Glw.PostRedisplay;
      end;
   K_Enter:
     case CurrentMenu of
      miReadDocs: ShowHelpMessage;
      miPlaySample1: NewGame(
        TLevel.Create('basic_castle_final.wrl', 'basic_castle_lights.wrl'));
      miPlaySample2: NewGame(TCastleHallLevel.Create);
      miViewGameMessages: ViewGameMessages;
      miSound:
        begin
          MessageOK(Glw,
            'Sound library (OpenAL) status:' +nl+
            nl+
            SoundInitializationReport +nl+
            nl+
            'TODO: for now, "The Castle" initializes OpenAL '+
            'but it''s not used. It will be used in the future, '+
            'and you will see here some controls to turn sound on/off '+
            'and change sound volume. See my older demo, ' +
            '[http://www.camelot.homedns.org/~michalis/lets_take_a_walk.php] '+
            'if you want to see how I''m dealing with OpenAL.',
            taLeft);
        end;
      miQuit: UserQuit := true;
      else raise EInternalError.Create('Menu item unknown');
     end;
  end;
end;

procedure ShowMenu;
var
  SavedMode: TGLMode;
begin
  SavedMode := TGLMode.Create(glw, 0);
  try
    SetStandardGLWindowState(Glw, Draw, nil{TODO CloseQuery}, Resize,
      nil, false, false, false, K_None, #0, false, false);

    Glw.OnKeyDown := KeyDown;

    Glw.EventResize;

    UserQuit := false;

    repeat
      Glwm.ProcessMessage(true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

procedure InitGLW(Glwin: TGLWindow);
begin
  ListBgDraw := LoadImageToDispList(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'menu_bg.png',
    [TRGBImage], [], Glw.Width, Glw.Height);
  MenuFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure CloseGLW(Flwin: TGLWindow);
begin
  FreeAndNil(MenuFont);
end;

initialization
 Glw.OnInitList.AppendItem(@InitGLW);
 Glw.OnCloseList.AppendItem(@CloseGLW);
end.