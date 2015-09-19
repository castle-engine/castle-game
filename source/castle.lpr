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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{$apptype GUI}

program castle;

{$ifdef MSWINDOWS}
  {$R ../automatic-windows-resources.res}
{$endif MSWINDOWS}

uses CastleWindow, SysUtils, CastleUtils, CastleProgress, CastleWindowProgress,
  Classes, CastleParameters, CastleMessages, CastleGLUtils, CastleStringUtils,
  CastleLog, GameWindow, GameStartMenu, GameHelp, CastleFilesUtils,
  CastleClassUtils, GameVideoOptions, GameInitialBackground,
  GameCreatures, GamePlay, GameGeneralMenu, CastleLevels, CastleMaterialProperties,
  CastleSoundEngine, CastleConfig, CastleRenderer, CastleResources, GameItems,
  CastleGameNotifications;

{ suggested screen size ------------------------------------------------------ }

const
  DefaultScreenWidth = 800;
  DefaultScreenHeight = 600;

  WindowParameters = [poDisplay, poGeometry];

{ parsing parameters --------------------------------------------------------- }

const
  Options: array [0..5] of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short: #0; Long: 'debug-no-creatures'; Argument: oaNone),
    (Short: #0; Long: 'debug-log'; Argument: oaNone),
    (Short: #0; Long: 'debug-log-cache'; Argument: oaNone),
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
           nl+
           Window.ParseParametersHelp(WindowParameters, true) +nl+
           nl+
           'Debug options (don''t use unless you know what you''re doing):' +nl+
           '  --debug-log           Write various log info on stdout' +nl+
           '  --debug-log-cache     Write log info, including cache, on stdout' +nl+
           '  --debug-menu-designer   Run menus is designer mode');
         Halt;
       end;
    1: begin
         WritelnStr(Version);
         Halt;
       end;
    2: { --debug-no-creatures not implemented for now };
    3: InitializeLog(Version);
    4: begin
         InitializeLog(Version);
         LogRendererCache := true;
       end;
    5: DebugMenuDesigner := true;
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
    - ApplicationConfig and ApplicationData use this. }
  OnGetApplicationName := @MyGetApplicationName;
  Config.Load;

  { configure Notifications }
  Notifications.CollectHistory := true;

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
  SoundEngine.ParseParameters;
  Window.ParseParameters(WindowParameters);
  Parameters.Parse(Options, @OptionProc, nil);

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

  { init progress }
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { load game data from XML files }
  MaterialProperties.URL := ApplicationData('textures/material_properties.xml');
  Resources.LoadFromFiles;
  ItemsResourcesInit;
  CreaturesResourcesInit;
  Levels.LoadFromFiles;
  Levels.LoadFromConfig;

  { init OpenAL (after initing Window and Progress, because ALContextOpen
    wants to display progress of "Loading sounds") }
  RenderInitialBackground;
  SoundEngine.ALContextOpen;

  ShowStartMenu;

  Config.Save;
end.
