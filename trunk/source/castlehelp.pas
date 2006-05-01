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
unit CastleHelp;

interface

const
  Version = '0.5.7';

procedure ShowCreditsMessage;

function SProgramHelpSuffix: string;

procedure ViewGameMessages;

implementation

uses SysUtils, Classes, GLWinMessages, CastleWindow, KambiUtils, CastlePlay;

procedure ShowCreditsMessage;
begin
  { TODO: put here credits from README etc. }
  MessageOK(Glw, SProgramHelpSuffix, taLeft);
end;

function SProgramHelpSuffix: string;
begin
  Result :=
    ApplicationName + ' version ' + Version + '.' +nl+
    'WWW: http://www.camelot.homedns.org/~michalis/castle.php' +nl+
    nl+
    'Author: Michalis Kamburelis, aka Kambi <michalis@camelot.homedns.org>' +nl+
    nl+
    'Compiled with ' + SCompilerDescription +'.';
end;

procedure ViewGameMessages;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Assign(GameMessages);
    SList.Insert(0, Format('%d messages :', [GameMessages.Count]));
    SList.Insert(1, '');
    MessageOK(Glw, SList, taLeft);
  finally SList.Free end;
end;

end.