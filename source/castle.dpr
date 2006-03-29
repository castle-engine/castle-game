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
  CastleWindow, CastleMenu, CastleLevel, CastleHelp, CastleSound,
  KambiClassUtils;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoSound: boolean = false;
  WasParam_NoScreenResize: boolean = false;

const
  Options: array[0..3]of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'no-sound'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-resize'; Argument: oaNone)
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
           '  -n / --no-screen-resize' +nl+
           '                        Do not try to resize the screen.' +nl+
           '                        If your screen size is not ' +
             RequiredScreenSize +nl+
           '                        then will run in windowed mode.' +nl+
           nl+
           SProgramHelpSuffix);
         ProgramBreak;
       end;
    1: WasParam_NoSound := true;
    2: begin
         WritelnStr(Version);
         ProgramBreak;
       end;
    3: WasParam_NoScreenResize := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'castle';
end;

{ main -------------------------------------------------------------------- }

begin
  { This is needed because
    - I sometimes display ApplicationName for user, and under Windows
      ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unsure for Unixes.
    - ParamStr(0) is useless for upx executables. }
  OnGetApplicationName := MyGetApplicationName;

  try
    { parse parameters }
    OpenALOptionsParse;
    ParseParameters(Options, OptionProc, nil);

    Glw.Width := RequiredScreenWidth;
    Glw.Height := RequiredScreenHeight;
    if WasParam_NoScreenResize then
    begin
      Glw.FullScreen :=
        (Glwm.ScreenWidth = RequiredScreenWidth) and
        (Glwm.ScreenHeight = RequiredScreenHeight);
    end else
    begin
      Glw.FullScreen := true;
      if (Glwm.ScreenWidth <> RequiredScreenWidth) or
         (Glwm.ScreenHeight <> RequiredScreenHeight) then
      begin
        Glwm.VideoResize := true;
        Glwm.VideoResizeWidth := RequiredScreenWidth;
        Glwm.VideoResizeHeight := RequiredScreenHeight;

        if Glwm.VideoResize then
          if not Glwm.TryVideoChange then
          begin
            WarningWrite('Can''t change display settings to ' +
              RequiredScreenSize + '. Will continue in windowed mode.');
            Glw.FullScreen := false;
          end;
      end;
    end;

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
    { I'm turning UseDescribePosition to false, because it's usually
      confusing for the user.
      E.g. each creature is conted as PrepareRenderSteps steps,
      each item is conted as PrepareRenderSteps steps,
      when loading levels user would have to know what an "octree" is. }
    Progress.UseDescribePosition := false;

    ShowMenu;
  finally
    EndAL;
  end;
end.

{
  Local Variables:
  compile-command: "fpcdebug castle.dpr --exe-output-dir ../"
  kam-compile-release-command-win32: "clean_glwindow_unit; fpcrelease --exe-output-dir ../"
  kam-compile-release-command-unix: "clean_glwindow_unit; fpcrelease -dGLWINDOW_XLIB --exe-output-dir ../"
  End:
}