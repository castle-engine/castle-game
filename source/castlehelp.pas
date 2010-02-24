{
  Copyright 2006-2010 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ }
unit CastleHelp;

interface

const
  Version = '0.8.3';

function SCastleVersion: string;
function SCastleWWW: string;

procedure ViewGameMessages;

implementation

uses SysUtils, Classes, GLWinMessages, CastleWindow, KambiUtils,
  CastleTimeMessages;

function SCastleVersion: string;
begin
  Result := ApplicationName + ' version ' + Version + '.';
end;

function SCastleWWW: string;
begin
  Result := 'WWW: http://vrmlengine.sourceforge.net/castle.php';
end;

procedure ViewGameMessages;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Assign(TimeMessagesList);
    SList.Insert(0, Format('%d messages :', [TimeMessagesList.Count]));
    SList.Insert(1, '');
    MessageOK(Glw, SList, taLeft);
  finally SList.Free end;
end;

end.