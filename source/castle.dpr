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

{$apptype GUI}

program castle;

uses GLWindow, SysUtils, KambiUtils, ProgressUnit, ProgressGL, OpenAL, ALUtils,
  Classes, ParseParametersUnit, GLWinMessages, KambiGLUtils, KambiStringUtils,
  KambiTimeUtils, KambiLog,
  CastleWindow, CastleStartMenu, CastleLevel, CastleHelp, CastleSound,
  KambiClassUtils, CastleVideoOptions, CastleInitialBackground,
  CastleCreatures, CastleObjectKinds, CastlePlay, CastleGeneralMenu;

{ requested screen size ------------------------------------------------------ }

const
  DefaultRequestedScreenWidth = 800;
  DefaultRequestedScreenHeight = 600;

var
  RequestedScreenWidth: Integer = DefaultRequestedScreenWidth;
  RequestedScreenHeight: Integer = DefaultRequestedScreenHeight;

function RequestedScreenSize: string;
begin
  Result := Format('%dx%d', [RequestedScreenWidth, RequestedScreenHeight]);
end;

function DefaultRequestedScreenSize: string;
begin
  Result := Format('%dx%d',
    [DefaultRequestedScreenWidth, DefaultRequestedScreenHeight]);
end;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoSound: boolean = false;
  WasParam_NoScreenChange: boolean = false;

const
  Options: array[0..8]of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short: #0; Long: 'no-sound'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-change'; Argument: oaNone),
    (Short: #0; Long: 'no-shadows'; Argument: oaNone),
    (Short: #0; Long: 'debug-no-creatures'; Argument: oaNone),
    (Short: #0; Long: 'debug-log'; Argument: oaNone),
    (Short: #0; Long: 'screen-size'; Argument: oaRequired),
    (Short: #0; Long: 'debug-menu-designer'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           SCastleVersionWWW +nl+
           nl+
           'Options:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           OpenALOptionsHelp(true) +nl+
           '  --no-sound            Turn off sound' +nl+
           '  -n / --no-screen-resize' +nl+
           '                        Do not try to resize the screen.' +nl+
           '                        If your screen size is not the required' +nl+
           '                        size (set by --screen-size)' +nl+
           '                        then will run in windowed mode.' +nl+
           '  --no-shadows          Disable initializing and using shadows.' +nl+
           '  --screen-size WIDTHxHEIGHT' +nl+
           '                        Change the screen size (default is ' +
             DefaultRequestedScreenSize + ').' +nl+
           nl+
           Glw.ParseParametersHelp([poDisplay], true) +nl+
           nl+
           'Debug options (don''t use unless you know what you''re doing):' +nl+
           '  --debug-log           Write various log info on stdout' +nl+
           '  --debug-no-creatures  Creatures animations will be loaded' +nl+
           '                        only when you explicitly request them' +nl+
           '                        from debug menu' +nl+
           '  --debug-menu-designer   Run menus is designer mode');
         ProgramBreak;
       end;
    1: WasParam_NoSound := true;
    2: begin
         WritelnStr(Version);
         ProgramBreak;
       end;
    3: WasParam_NoScreenChange := true;
    4: RenderShadowsPossible := false;
    5: WasParam_DebugNoCreatures := true;
    6: InitializeLog(Version);
    7: begin
         DeFormat(Argument, '%dx%d',
           [@RequestedScreenWidth, @RequestedScreenHeight]);
       end;
    8: DebugMenuDesigner := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main -------------------------------------------------------------------- }

begin
  { parse parameters }
  OpenALOptionsParse;
  Glw.ParseParameters([poDisplay]);
  ParseParameters(Options, @OptionProc, nil);

  Glw.Width := RequestedScreenWidth;
  Glw.Height := RequestedScreenHeight;
  Glw.ColorBits := ColorDepthBits;
  if WasParam_NoScreenChange or (not AllowScreenChange) then
  begin
    Glw.FullScreen :=
      (Glwm.ScreenWidth = RequestedScreenWidth) and
      (Glwm.ScreenHeight = RequestedScreenHeight);
  end else
  begin
    Glw.FullScreen := true;
    if (Glwm.ScreenWidth <> RequestedScreenWidth) or
       (Glwm.ScreenHeight <> RequestedScreenHeight) or
       (VideoFrequency <> 0) or
       (ColorDepthBits <> 0) then
    begin
      Glwm.VideoColorBits := ColorDepthBits;
      Glwm.VideoFrequency := VideoFrequency;
      Glwm.VideoResize := true;
      Glwm.VideoResizeWidth := RequestedScreenWidth;
      Glwm.VideoResizeHeight := RequestedScreenHeight;

      if not Glwm.TryVideoChange then
      begin
        WarningWrite('Can''t change display settings to: ' +nl+
          Glwm.VideoSettingsDescribe +
          nl+
          'I will set "Allow screen settings change on startup" option to "No". ' +
          'You may want to review settings in "Video options" menu and then ' +
          'set "Allow screen settings change on startup" back to "Yes".' +nl+
          nl+
          'Now I will just continue with default system settings. ');
        Glw.FullScreen :=
          (Glwm.ScreenWidth = RequestedScreenWidth) and
          (Glwm.ScreenHeight = RequestedScreenHeight);
        AllowScreenChange := false;
      end;
    end;
  end;

  { init glwindow }
  Glw.Caption := 'The Castle';
  Glw.ResizeAllowed := raOnlyAtInit;
  if RenderShadowsPossible then
    Glw.StencilBufferBits := 8;
  Glw.Init;

  { init progress }
  ProgressGLInterface.Window := Glw;
  Progress.UserInterface := ProgressGLInterface;
  { I'm turning UseDescribePosition to false, because it's usually
    confusing for the user.
    E.g. each creature is counted as PrepareRenderSteps steps,
    each item is counted as PrepareRenderSteps steps,
    when loading levels user would have to know what an "octree" is. }
  Progress.UseDescribePosition := false;

  { init OpenAL (after initing Glw and Progress, because ALContextInit
    wants to display progress of "Loading sounds") }
  DrawInitialBackground;
  SoundEngine.ALContextInit(WasParam_NoSound);
  try
    ShowStartMenu;
  finally
    { Usually Glw.Closed = false here.
      But this is finally...end clause so we try hard to avoid raising
      another exception here --- so we safeguard and eventually change
      Progress.UserInterface here. }
    if Glw.Closed then
      Progress.UserInterface := ProgressNullInterface;

    SoundEngine.ALContextClose;
  end;
end.

{
  Local Variables:
  compile-command: "fpcdebug castle.dpr --exe-output-dir ../"
  kam-compile-release-command-unix:    "make -C ../ build-unix    && mv -fv ../castle      ~/bin/"
  kam-compile-release-command-windows: "make -C ../ build-windows && mv -fv ../castle.exe c:/bin/"
  End:
}