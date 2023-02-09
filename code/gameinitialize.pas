{
  Copyright 2006-2023 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Implements the game logic, independent from mobile / standalone. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  { CGE units }
  CastleWindow, CastleUtils, CastleProgress, CastleWindowProgress,
  CastleParameters, CastleMessages, CastleGLUtils, CastleStringUtils,
  CastleLog, CastleClassUtils, CastleLevels, CastleMaterialProperties,
  CastleSoundEngine, CastleConfig, CastleResources,
  CastleGameNotifications, CastleInputs, CastleRectangles, CastleColors,
  CastleImages, CastleFilesUtils,
  { castle GameXxx units }
  GameWindow, GameHelp,  GameVideoOptions, GameItems, GameCreatures,
  GameGeneralMenu, GameSound
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStartMenu
  , GameCredits
  , GameDebugMenu
  , GameControlsMenu
  , GameGameMenu
  , GamePlay
  , GameChooseMenu
  {$endregion 'Castle Initialization Uses'};

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  UserConfig.Load;
  SoundEngine.LoadFromConfig(UserConfig);
  InputsAll.LoadFromConfig(UserConfig);

  { parse SoundEngine parameters, should override things read by
    SoundEngine.LoadFromConfig }
  SoundEngine.ParseParameters;

  { configure Notifications }
  Notifications.CollectHistory := true;
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Color := Yellow;

  { configure progress }
  Progress.UserInterface := WindowProgressInterface;
  Progress.UserInterface.Image := LoadImage('castle-data:/menu_bg/initial_background.png');
  Progress.UserInterface.OwnsImage := true;

  { set sound configuration. Must be done before loading resources,
    since they initialize the sounds by looking up their names. }
  InitializeSound;

  { load game data from XML files }
  MaterialProperties.URL := 'castle-data:/textures/material_properties.xml';
  Resources.LoadFromFiles;
  ItemsResourcesInit;
  CreaturesResourcesInit;
  Levels.LoadFromFiles;
  Levels.SortByNumber;
  Levels.LoadFromConfig(UserConfig);

  { Create game states and set initial state }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StateStartMenu := TStateStartMenu.Create(Application);
  StateCredits := TStateCredits.Create(Application);
  StateDebugMenu := TStateDebugMenu.Create(Application);
  StateControlsMenu := TStateControlsMenu.Create(Application);
  StateGameMenu := TStateGameMenu.Create(Application);
  StatePlay:= TStatePlay.Create(Application);
  StateChooseMenu := TStateChooseMenu.Create(Application);
  {$endregion 'Castle State Creation'}

  Window.Container.View := StateStartMenu;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TGameWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }
  Window.FullScreen := true;

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
finalization
  SoundEngine.SaveToConfig(UserConfig);
  InputsAll.SaveToConfig(UserConfig);
  Levels.SaveToConfig(UserConfig);
  UserConfig.Save;
end.
