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
unit KambiXMLCfg;

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

implementation

uses SysUtils;

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

end.