unit CastleHelp;

interface

const
  Version = '0.3.0';

procedure ShowHelpMessage;

function SProgramHelpSuffix: string;

procedure ViewGameMessages;

implementation

uses SysUtils, Classes, GLWinMessages, CastleWindow, KambiUtils, CastlePlay;

procedure ShowHelpMessage;
const
  HelpMessage = {$I castle_help_message.inc};
begin
  MessageOK(Glw, HelpMessage + nl + SProgramHelpSuffix, taLeft);
end;

function SProgramHelpSuffix: string;
begin
  Result :=
    ApplicationName + ' version ' + Version + '.' +nl+
    'Author: Michalis Kamburelis, aka Kambi <michalis@camelot.homedns.org>' +nl+
    { TODO: later I will just use here SCamelotProgramHelpSuffix,
      for now this program is not avail on camelot. }
    {'See http://www.camelot.homedns.org/~michalis/ for latest versions' +
    Iff(WrapLines, nl + ' ', '') +
    ' of this program, sources, documentation etc.' +nl+}
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