{
  Copyright 2006-2016 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit GameHelp;

interface

const
  Version = '1.0.1';
  CastleURL = 'http://castle-engine.sourceforge.net/castle.php';

function SCastleVersion: string;

procedure ViewGameMessages;

implementation

uses SysUtils, Classes, CastleMessages, GameWindow, CastleUtils,
  CastleGameNotifications;

function SCastleVersion: string;
begin
  Result := ApplicationName + ' version ' + Version + '.';
end;

procedure ViewGameMessages;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Assign(Notifications.History);
    SList.Insert(0, Format('%d messages :', [Notifications.History.Count]));
    SList.Insert(1, '');
    MessageOK(Window, SList);
  finally SList.Free end;
end;

end.
