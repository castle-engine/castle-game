unit CastleLog;

interface

type
  TLogType = (
    ltSoundInitialization,
    ltOpenGLInitialization,
    ltTimeMessage,
    ltGLWinMessage,
    ltAnimationInfo
  );

var
  WasParam_DebugLog: boolean = false;

procedure InitializeLog;

procedure WritelnLog(const LogType: TLogType; const LogMessage: string);

implementation

uses Classes, KambiUtils, KambiClassUtils, GLWinMessages;

procedure GLWinMessageLog(Text: TStringList);
begin
  WritelnLog(ltGLWinMessage, Text.Text);
end;

procedure InitializeLog;
begin
  WasParam_DebugLog := true;
  if StdOutStream = nil then
    raise EWithHiddenClassName.Create(
      'You used --debug-log option but stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "castle > castle.log".');
  OnGLWinMessage := GLWinMessageLog;
end;

procedure WritelnLog(const LogType: TLogType; const LogMessage: string);
const
  LogTypeName: array[TLogType] of string =
  ( 'Sound initialization', 'OpenGL initialization', 'Time message', 'Message',
    'Animation info' );
  LogMultiline: set of TLogType =
  [ ltSoundInitialization, ltOpenGLInitialization, ltGLWinMessage ];
begin
  if LogType in LogMultiline then
  begin
    WritelnStr('-------------------- ' + LogTypeName[LogType] + ': ' + NL +
      LogMessage + NL +
      '--------------------');
  end else
    WritelnStr(LogTypeName[LogType] + ': ' + LogMessage);
end;

end.