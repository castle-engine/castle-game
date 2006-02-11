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

{$apptype GUI}

program castle;

uses GLWindow, SysUtils, KambiUtils,
  ProgressUnit, ProgressGL, OpenAL, ALUtils,
  ParseParametersUnit, GLWinMessages, KambiGLUtils,
  CastleWindow, CastleMenu, CastleLevel, CastlePlay, CastleSound;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoSound: boolean = false;

const
  Options: array[0..2]of TOption =
  ((Short:'h'; Long: 'help'; Argument: oaNone),
   (Short: #0; Long: 'no-sound'; Argument: oaNone),
   (Short:'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
 case OptionNum of
  0: begin
       InfoWrite(
         'castle.' +nl+
         nl+
         'Options:' +nl+
         HelpOptionHelp +nl+
         VersionOptionHelp +nl+
         OpenALOptionsHelp(true) +nl+
         '  --no-sound            Turn off sound' +nl+
         nl+
         TGLWindow.ParseParametersHelp(StandardParseOptions, true) +nl+
         nl+
         SCamelotProgramHelpSuffix(DisplayProgramName, Version, true));
       ProgramBreak;
     end;
  1: WasParam_NoSound := true;
  2: begin
       Writeln(Version);
       ProgramBreak;
     end;
 end;
end;

{ main -------------------------------------------------------------------- }

begin
  try
    { parse parameters }
    Glw.FullScreen := true; { by default we open in fullscreen }
    Glw.ParseParameters(StandardParseOptions);
    OpenALOptionsParse;
    ParseParameters(Options, OptionProc, nil);

    { init OpenAL }
    if WasParam_NoSound then
      SoundInitializationReport :=
        'Sound disabled by --no-sound command-line option' else
    if not TryBeginAL(false) then
      SoundInitializationReport :=
        'OpenAL initialization failed : ' +ALActivationErrorMessage +nl+
        'SOUND IS DISABLED' else
      SoundInitializationReport :=
        'OpenAL initialized, sound enabled';

    { init glwindow }
    Glw.Caption := 'The Castle';
    Glw.ResizeAllowed := raOnlyAtInit;
    Glw.Init;

    { init progress }
    ProgressGLInterface.Window := Glw;
    Progress.UserInterface := ProgressGLInterface;

    ShowMenu;
  finally
    EndAL;
  end;
end.

{
  Local Variables:
  kam-compile-release-command-win32: "clean_glwindow_unit; fpcrelease"
  kam-compile-release-command-unix: "clean_glwindow_unit; fpcreleaseb -dGLWINDOW_XLIB"
  End:
}