{ User config file.
  Note that this unit initializes OnGetApplicationName in initialization. }
unit CastleConfig;

interface

uses XMLCfg;

var
  { User config file for this game.
    Will be created (and FileName set) in initialization,
    will be flushed and freed in finalization. }
  ConfigFile: TXMLConfig;

implementation

uses SysUtils, KambiFilesUtils;

function MyGetApplicationName: string;
begin
  Result := 'castle';
end;

initialization
  { This is needed because
    - I sometimes display ApplicationName for user, and under Windows
      ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unsure for Unixes.
    - ParamStr(0) is useless for upx executables. }
  OnGetApplicationName := MyGetApplicationName;

  ConfigFile := TXMLConfig.Create(nil);
  ConfigFile.FileName := UserConfigFile('.conf');
finalization
  if ConfigFile <> nil then
  begin
    ConfigFile.Flush;
    FreeAndNil(ConfigFile);
  end;
end.