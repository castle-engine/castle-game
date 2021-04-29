{
  Copyright 2006-2021 Michalis Kamburelis.

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
  CastleSoundEngine, CastleConfig, CastleUIState, CastleResources,
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
  Progress.UserInterface.Image :=
    LoadImage(ApplicationData('menu_bg/initial_background.png'));
  Progress.UserInterface.OwnsImage := true;

  { set sound configuration. Must be done before loading resources,
    since they initialize the sounds by looking up their names. }
  InitializeSound;

  { load game data from XML files }
  MaterialProperties.URL := ApplicationData('textures/material_properties.xml');
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

  TUIState.Current := StateStartMenu;
end;

function MyGetApplicationName: string;
begin
  Result := 'castle';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  { Hack to not activate log yet on desktops,
    only to make --version command-line parameter working without any extra output }
  {$ifndef LINUX}
  {$ifndef MSWINDOWS}
  InitializeLog;
  {$endif}
  {$endif}

  Application.OnInitialize := @ApplicationInitialize;
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { for state credits (scrolling text animation), for state game }
  Window.AutoRedisplay := true;
end.
