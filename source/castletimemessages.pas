{
  Copyright 2006,2007 Michalis Kamburelis.

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
unit CastleTimeMessages;

interface

uses TimeMessages, Classes;

var
  { These are all messages passed to TimeMessage.
    Created / destroyed in this unit's initialization / finalization.
    They are not broken (to fit into some particular line width).

    You should clear this when new game starts (use TimeMessagesClear).

    You cannot modify it directly, you can change it
    only by calling TimeMessage. }
  TimeMessagesList: TStringList;

{ Add message to TimeMessages and TimeMessagesManager. }
procedure TimeMessage(const S: string);

{ This must be called within 2d projection. }
procedure TimeMessagesDraw;

function TimeMessagesDrawNeeded: boolean;

procedure TimeMessagesClear;

procedure TimeMessagesIdle;

implementation

uses SysUtils, GLWindow, CastleWindow, KambiClassUtils, CastleLog;

var
  TimeMessagesManager: TTimeMessagesManager;

procedure TimeMessage(const S: string);
begin
  if TimeMessagesManager <> nil then
    TimeMessagesManager.Show(S);
  TimeMessagesList.Insert(0, S);

  if WasParam_DebugLog then
    WritelnLog(ltTimeMessage, S);
end;

procedure TimeMessagesDraw;
begin
  TimeMessagesManager.Draw2d(Glw.Width, Glw.Height,
                             Glw.Width, Glw.Height);
end;

function TimeMessagesDrawNeeded: boolean;
begin
  Result := TimeMessagesManager.DrawNeeded;
end;

procedure TimeMessagesClear;
begin
  TimeMessagesList.Clear;
  TimeMessagesManager.Clear;
end;

procedure TimeMessagesIdle;
begin
  TimeMessagesManager.Idle;
end;

{ initialization / finalization ---------------------------------------------- }

procedure GLWindowInit(Glwin: TGLWindow);
begin
  TimeMessagesManager := TTimeMessagesManager.Create(
    Glw, hpMiddle, vpDown, Glw.Width);
  TimeMessagesManager.MaxMessagesCount := 4;
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  FreeAndNil(TimeMessagesManager);
end;

initialization
  TimeMessagesList := TStringList.Create;

  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
finalization
  FreeAndNil(TimeMessagesList);
end.