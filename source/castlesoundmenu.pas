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

{ Some things common for "Sound options" menu in start menu and game menu. }
unit CastleSoundMenu;

interface

uses GLMenu;

type
  TSoundVolumeSlider = class(TGLMenuFloatSlider)
    constructor Create(const AValue: Single);
    function ValueToStr(const AValue: Single): string; override;
  end;

procedure ViewSoundInfo;

implementation

uses Classes, SysUtils, GLWindow, CastleWindow, CastleSound,
  KambiClassUtils, KambiUtils, OpenAL, ALUtils, GLWinMessages;

procedure ViewSoundInfo;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Append('Sound library (OpenAL) status:');
    S.Append('');
    Strings_AddSplittedString(S, SoundEngine.SoundInitializationReport, nl);
    SoundEngine.AppendALInformation(S);

    MessageOK(Glw, S, taLeft);
  finally S.Free end;
end;

{ TSoundVolumeSlider ---------------------------------------------------- }

constructor TSoundVolumeSlider.Create(const AValue: Single);
begin
  inherited Create(0, 1, AValue);
end;

function TSoundVolumeSlider.ValueToStr(const AValue: Single): string;
begin
  if AValue = 0.0 then
    Result := 'Off' else
    Result := inherited ValueToStr(AValue);
end;

end.