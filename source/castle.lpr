{
  Copyright 2006-2011 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{$apptype GUI}

program castle;

uses GLWindow, SysUtils, KambiUtils, ProgressUnit, GLProgress,
  Classes, ParseParametersUnit, GLWinMessages, KambiGLUtils, KambiStringUtils,
  KambiLog, CastleWindow, CastleStartMenu, CastleHelp, CastleSound,
  KambiClassUtils, CastleVideoOptions, CastleInitialBackground,
  CastleCreatures, CastlePlay, CastleGeneralMenu,
  CastleRequiredResources, CastleCredits, GLAntiAliasing, ALSoundEngine,
  VRMLGLRenderer;

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
  WasParam_NoScreenChange: boolean = false;

const
  Options: array[0..8]of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-change'; Argument: oaNone),
    (Short: #0; Long: 'no-shadows'; Argument: oaNone),
    (Short: #0; Long: 'debug-no-creatures'; Argument: oaNone),
    (Short: #0; Long: 'debug-log'; Argument: oaNone),
    (Short: #0; Long: 'debug-log-cache'; Argument: oaNone),
    (Short: #0; Long: 'screen-size'; Argument: oaRequired),
    (Short: #0; Long: 'debug-menu-designer'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           SCastleVersion +nl+
           SCastleWWW +nl+
           nl+
           'Options:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           SoundEngine.ParseParametersHelp +nl+
           '  -n / --no-screen-resize' +nl+
           '                        Do not try to resize the screen.' +nl+
           '                        If your screen size is not the required' +nl+
           '                        size (set by --screen-size)' +nl+
           '                        then run in windowed mode.' +nl+
           '  --no-shadows          Disable initializing and using shadows.' +nl+
           '  --screen-size WIDTHxHEIGHT' +nl+
           '                        Change the screen size (default is ' +
             DefaultRequestedScreenSize + ').' +nl+
           nl+
           Window.ParseParametersHelp([poDisplay], true) +nl+
           nl+
           'Debug options (don''t use unless you know what you''re doing):' +nl+
           '  --debug-log           Write various log info on stdout' +nl+
           '  --debug-log-cache     Write log info, including cache, on stdout' +nl+
           '  --debug-no-creatures  Creatures animations will be loaded' +nl+
           '                        only when you explicitly request them' +nl+
           '                        from debug menu' +nl+
           '  --debug-menu-designer   Run menus is designer mode');
         ProgramBreak;
       end;
    1: begin
         WritelnStr(Version);
         ProgramBreak;
       end;
    2: WasParam_NoScreenChange := true;
    3: RenderShadowsPossible := false;
    4: DebugNoCreatures := true;
    5: InitializeLog(Version);
    6: begin
         InitializeLog(Version);
         LogRendererCache := true;
       end;
    7: begin
         DeFormat(Argument, '%dx%d',
           [@RequestedScreenWidth, @RequestedScreenHeight]);
       end;
    8: DebugMenuDesigner := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ initializing GL context --------------------------------------------------- }

procedure MultiSamplingOff(Window: TGLWindow; const FailureMessage: string);
begin
  AntiAliasing := 0;
  if Log then WritelnLogMultiline('GL context', FailureMessage);
end;

procedure StencilOff(Window: TGLWindow; const FailureMessage: string);
begin
  RenderShadowsPossible := false;
  if Log then WritelnLogMultiline('GL context', FailureMessage);
end;

{ Call Window.Open, when anti-aliasing (multi-sampling) and shadows (stencil
  buffer) are possibly allowed. If EGLContextNotPossible, will try to lower
  requirements and initialize worse GL context. }
procedure OpenContext;
begin
  Window.OpenOptionalMultiSamplingAndStencil(@MultiSamplingOff, @StencilOff);
end;

{ main -------------------------------------------------------------------- }

begin
  { parse parameters }
  SoundEngine.ParseParameters;
  Window.ParseParameters([poDisplay]);
  ParseParameters(Options, @OptionProc, nil);

  Window.Width := RequestedScreenWidth;
  Window.Height := RequestedScreenHeight;
  Window.ColorBits := ColorDepthBits;
  if WasParam_NoScreenChange or (not AllowScreenChange) then
  begin
    Window.FullScreen :=
      (Application.ScreenWidth = RequestedScreenWidth) and
      (Application.ScreenHeight = RequestedScreenHeight);
  end else
  begin
    Window.FullScreen := true;
    if (Application.ScreenWidth <> RequestedScreenWidth) or
       (Application.ScreenHeight <> RequestedScreenHeight) or
       (VideoFrequency <> 0) or
       (ColorDepthBits <> 0) then
    begin
      Application.VideoColorBits := ColorDepthBits;
      Application.VideoFrequency := VideoFrequency;
      Application.VideoResize := true;
      Application.VideoResizeWidth := RequestedScreenWidth;
      Application.VideoResizeHeight := RequestedScreenHeight;

      if not Application.TryVideoChange then
      begin
        WarningWrite('Can''t change display settings to: ' +nl+
          Application.VideoSettingsDescribe +
          nl+
          'I will set "Allow screen settings change on startup" option to "No". ' +
          'You may want to review settings in "Video options" menu and then ' +
          'set "Allow screen settings change on startup" back to "Yes".' +nl+
          nl+
          'Now I will just continue with default system settings. ');
        Window.FullScreen :=
          (Application.ScreenWidth = RequestedScreenWidth) and
          (Application.ScreenHeight = RequestedScreenHeight);
        AllowScreenChange := false;
      end;
    end;
  end;

  { init glwindow }
  Window.Caption := 'The Castle';
  Window.ResizeAllowed := raOnlyAtOpen;
  if RenderShadowsPossible then
    Window.StencilBufferBits := 8;
  Window.MultiSampling := AntiAliasingGlwMultiSampling;
  OpenContext;

  { init progress }
  GLProgressInterface.Window := Window;
  Progress.UserInterface := GLProgressInterface;
  { I'm turning UseDescribePosition to false, because it's usually
    confusing for the user.
    E.g. each creature is counted as PrepareRenderSteps steps,
    each item is counted as PrepareRenderSteps steps,
    when loading levels user would have to know what an "octree" is. }
  Progress.UseDescribePosition := false;

  { init OpenAL (after initing Glw and Progress, because ALContextOpen
    wants to display progress of "Loading sounds") }
  DrawInitialBackground;
  SoundEngine.ALContextOpen;
  try
    ShowStartMenu;
  finally
    CredistGLContextRelease;

    { Usually Window.Closed = false here.
      But this is finally...end clause so we try hard to avoid raising
      another exception here --- so we safeguard and eventually change
      Progress.UserInterface here. }
    if Window.Closed then
      Progress.UserInterface := ProgressNullInterface;

    SoundEngine.ALContextClose;
  end;
end.

{
  Local Variables:
  compile-command: "fpcdebug castle.lpr --exe-output-dir ../"
  kam-compile-release-command-unix:    "make -C ../ build-unix    && mv -fv ../castle      ~/bin/"
  kam-compile-release-command-windows: "make -C ../ build-windows"
  End:
}