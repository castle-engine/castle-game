{
  Copyright 2006-2012 Michalis Kamburelis.

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

uses CastleWindow, SysUtils, CastleUtils, ProgressUnit, CastleProgress,
  Classes, CastleParameters, CastleMessages, CastleGLUtils, CastleStringUtils,
  CastleLog, GameWindow, GameStartMenu, GameHelp, CastleFilesUtils,
  CastleClassUtils, GameVideoOptions, GameInitialBackground,
  GameCreatures, GamePlay, GameGeneralMenu, CastleLevels, CastleTextureProperties,
  CastleSoundEngine, CastleConfig, GLRenderer, CastleResources, GameItems,
  CastleGameNotifications;

{ requested screen size ------------------------------------------------------ }

const
  DefaultRequestedScreenWidth = 800;
  DefaultRequestedScreenHeight = 600;

var
  RequestedScreenWidth: Integer = DefaultRequestedScreenWidth;
  RequestedScreenHeight: Integer = DefaultRequestedScreenHeight;

function DefaultRequestedScreenSize: string;
begin
  Result := Format('%dx%d',
    [DefaultRequestedScreenWidth, DefaultRequestedScreenHeight]);
end;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoScreenChange: boolean = false;

const
  Options: array [0..7] of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-change'; Argument: oaNone),
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
           'Website: ' + CastleURL +nl+
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
           '  --screen-size WIDTHxHEIGHT' +nl+
           '                        Change the screen size (default is ' +
             DefaultRequestedScreenSize + ').' +nl+
           nl+
           Window.ParseParametersHelp([poDisplay], true) +nl+
           nl+
           'Debug options (don''t use unless you know what you''re doing):' +nl+
           '  --debug-log           Write various log info on stdout' +nl+
           '  --debug-log-cache     Write log info, including cache, on stdout' +nl+
           '  --debug-menu-designer   Run menus is designer mode');
         ProgramBreak;
       end;
    1: begin
         WritelnStr(Version);
         ProgramBreak;
       end;
    2: WasParam_NoScreenChange := true;
    3: { --debug-no-creatures not implemented for now };
    4: InitializeLog(Version);
    5: begin
         InitializeLog(Version);
         LogRendererCache := true;
       end;
    6: begin
         DeFormat(Argument, '%dx%d',
           [@RequestedScreenWidth, @RequestedScreenHeight]);
       end;
    7: DebugMenuDesigner := true;
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
    - UserConfigFile uses this, determines Config.FileName. }
  OnGetApplicationName := @MyGetApplicationName;
  Config.Load;

  { configure Notifications }
  Notifications.CollectHistory := true;

  { parse parameters }
  SoundEngine.ParseParameters;
  Window.ParseParameters([poDisplay]);
  Parameters.Parse(Options, @OptionProc, nil);

  Window.Width := RequestedScreenWidth;
  Window.Height := RequestedScreenHeight;
  Window.ColorBits := ColorBits;
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
       (ColorBits <> 0) then
    begin
      Application.VideoColorBits := ColorBits;
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

  { init window }
  Window.Caption := 'The Castle';
  Window.ResizeAllowed := raOnlyAtOpen;
  Window.StencilBits := 8;
  Window.Open;

  { init progress }
  WindowProgressInterface.Window := Window;
  Progress.UserInterface := WindowProgressInterface;

  { load game data from XML files }
  TexturesProperties.XmlFileName := ProgramDataPath + 'data' +
    PathDelim + 'textures' + PathDelim + 'index.xml';
  Resources.LoadFromFiles;
  ItemsKindsInit;
  CreaturesKindsInit;
  Levels.LoadFromFiles;
  Levels.LoadFromConfig;

  { init OpenAL (after initing Glw and Progress, because ALContextOpen
    wants to display progress of "Loading sounds") }
  DrawInitialBackground;
  SoundEngine.ALContextOpen;
  try
    ShowStartMenu;
  finally
    { Usually Window.Closed = false here.
      But this is finally...end clause so we try hard to avoid raising
      another exception here --- so we safeguard and eventually change
      Progress.UserInterface here. }
    if Window.Closed then
      Progress.UserInterface := ProgressNullInterface;

    SoundEngine.ALContextClose;
  end;

  Config.Save;
end.

{
  Local Variables:
  compile-command: "fpcdebug castle.lpr --exe-output-dir ../"
  End:
}