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

{ User config file.
  Note that this unit initializes OnGetApplicationName in initialization. }
unit CastleConfig;

interface

uses KambiUtils, XMLCfg;

type
  { This is descendant of TXMLConfig that adds
    GetValue, SetValue, SetDeleteValue for the Float type. }
  TKamXMLConfig = class(TXMLConfig)
  public
    function GetValue(const APath: string;
      const ADefaultValue: Float): Float; overload;

    procedure SetValue(const APath: string;
      const AValue: Float); overload;

    procedure SetDeleteValue(const APath: string;
      const AValue, ADefaultValue: Float); overload;
  end;

var
  { User config file for this game.
    Will be created (and FileName set) in initialization,
    will be flushed and freed in finalization. }
  ConfigFile: TKamXMLConfig;

implementation

uses SysUtils, KambiFilesUtils;

{ TKamXMLConfig -------------------------------------------------------------- }

function TKamXMLConfig.GetValue(const APath: string;
  const ADefaultValue: Float): Float;
var
  ResultString: string;
begin
  ResultString := GetValue(APath, FloatToStr(ADefaultValue));
  Result := StrToFloatDef(ResultString, ADefaultValue);
end;

procedure TKamXMLConfig.SetValue(const APath: string;
  const AValue: Float);
begin
  SetValue(APath, FloatToStr(AValue));
end;

procedure TKamXMLConfig.SetDeleteValue(const APath: string;
  const AValue, ADefaultValue: Float);
begin
  SetDeleteValue(APath, FloatToStr(AValue), FloatToStr(ADefaultValue));
end;

{ initialization / finalization --------------------------------------------- }

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

  ConfigFile := TKamXMLConfig.Create(nil);
  ConfigFile.FileName := UserConfigFile('.conf');
finalization
  if ConfigFile <> nil then
  begin
    ConfigFile.Flush;
    FreeAndNil(ConfigFile);
  end;
end.