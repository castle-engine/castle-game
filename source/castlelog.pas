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

uses Classes, KambiUtils, KambiClassUtils, GLWinMessages, CastleHelp,
  SysUtils;

procedure GLWinMessageLog(Text: TStringList);
begin
  WritelnLog(ltGLWinMessage, Text.Text);
end;

procedure InitializeLog;

  procedure RaiseStdOutNotAvail;
  begin
    raise EWithHiddenClassName.Create(
      'You used --debug-log option but it seems that stdout (standard output) ' +
      'is not available. Under Windows you should explicitly ' +
      'redirect program''s stdout to make it available, e.g. ' +
      'run "castle --debug-log > castle.log".');
  end;

begin
  WasParam_DebugLog := true;

  { Ideally, check for "StdOutStream = nil" should be all that is needed,
    and wrapping WritelnStr inside try...except should not be needed.
    But... see StdOutStream comments: you cannot
    depend on the fact that "StdOutStream <> nil means that stdout
    is actually available (because user redirected stdout etc.). }

  if StdOutStream = nil then
    RaiseStdOutNotAvail;

  try
    WritelnStr('Log started on ' + DateTimeToAtStr(Now) + '. Version ' + Version);
  except
    on E: EWriteError do RaiseStdOutNotAvail;
  end;

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