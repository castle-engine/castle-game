{
  Copyright 2006-2014 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ----------------------------------------------------------------------------
}

{$apptype GUI}

program castle;

{$ifdef MSWINDOWS}
  {$R ../automatic-windows-resources.res}
{$endif MSWINDOWS}

uses SysUtils, Classes,
  { CGE units }
  CastleWindow, CastleUtils, CastleProgress, CastleWindowProgress,
  CastleParameters, CastleMessages, CastleGLUtils, CastleStringUtils,
  CastleLog, CastleClassUtils, CastleLevels, CastleMaterialProperties,
  CastleSoundEngine, CastleConfig, CastleRenderer, CastleResources,
  CastleGameNotifications, CastleInputs, CastleRectangles, CastleColors,
  CastleUIState, CastleFilesUtils,
  { castle GameXxx units }
  GameWindow, GameHelp, GameVideoOptions, GameControlsMenu,
  GameGeneralMenu, Game;

{ suggested screen size ------------------------------------------------------ }

const
  DefaultScreenWidth = 800;
  DefaultScreenHeight = 600;

  WindowParameters = [poDisplay, poGeometry];

{ parsing parameters --------------------------------------------------------- }

const
  Options: array [0..2] of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:#0 ; Long: 'touch'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           SCastleVersion +nl+
           'Website: ' + CastleURL +nl+
           nl+
           'Options:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           SoundEngine.ParseParametersHelp +nl+
           nl+
           Window.ParseParametersHelp(WindowParameters, true));
         Halt;
       end;
    1: begin
         WritelnStr(Version);
         Halt;
       end;
    2: UseTouchInterface := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main -------------------------------------------------------------------- }

begin
  { By default, run in FullScreen with DefaultScreenWidth x DefaultScreenHeight.
    User can change it with options like --geometry,
    see TCastleWindow.ParseParameters docs. }
  Window.FullScreen := true;
  Window.Width := DefaultScreenWidth;
  Window.Height := DefaultScreenHeight;
  { On some backends, setting Window.ColorBits is enough.
    On some backends, setting Application.VideoColorBits and
    doing Application.TryVideoChange is required (this is handled lower). }
  Window.ColorBits := ColorBits;

  { parse parameters }
  Window.ParseParameters(WindowParameters);
  Parameters.Parse(Options, @OptionProc, nil, true); // allow future SoundEngine.ParseParameters

  if AllowScreenChange and Window.FullScreen and
     ( (Application.ScreenWidth <> DefaultScreenWidth) or
       (Application.ScreenHeight <> DefaultScreenHeight) or
       (VideoFrequency <> 0) or
       (ColorBits <> 0) ) then
  begin
    Application.VideoColorBits := ColorBits;
    Application.VideoFrequency := VideoFrequency;
    Application.VideoResize := true;
    Application.VideoResizeWidth := DefaultScreenWidth;
    Application.VideoResizeHeight := DefaultScreenHeight;

    if not Application.TryVideoChange then
    begin
      WarningWrite('Can''t change display settings to: ' +nl+
        Application.VideoSettingsDescribe +
        nl+
        'I will set "Allow screen settings change on startup" option to "No". ' +
        'You may want to review settings in "Video options" menu and then ' +
        'set "Allow screen settings change on startup" back to "Yes".' +nl+
        nl+
        'Now I will just continue with default system screen size. ');
      AllowScreenChange := false;
    end;
  end;

  { init window }
  Window.Caption := 'The Castle';
  Window.ResizeAllowed := raOnlyAtOpen;
  Window.StencilBits := 8;
  Window.Open;

  Application.Run;

  { stopping any currently started state now.
    This makes destruction order predictable (as opposed to just letting
    TComponent destructors recursively call children destruction),
    and makes it easier to write crash-free exit. }
  TUIState.Current := nil;

  SoundEngine.SaveToConfig(UserConfig);
  InputsAll.SaveToConfig(UserConfig);
  Levels.SaveToConfig(UserConfig);
  UserConfig.Save;
end.
