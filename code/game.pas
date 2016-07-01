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

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

implementation

uses SysUtils, Classes,
  { CGE units }
  CastleWindow, CastleUtils, CastleProgress, CastleWindowProgress,
  CastleParameters, CastleMessages, CastleGLUtils, CastleStringUtils,
  CastleLog, CastleClassUtils, CastleLevels, CastleMaterialProperties,
  CastleSoundEngine, CastleConfig, CastleUIState, CastleResources,
  CastleGameNotifications, CastleInputs, CastleRectangles, CastleColors,
  CastleImages,
  { castle GameXxx units }
  GameWindow, GameStartMenu, GameHelp, CastleFilesUtils, GameControlsMenu,
  GameVideoOptions, GameItems, GameDebugMenu,
  GameCreatures, GamePlay, GameGeneralMenu, GameCredits, GameGameMenu,
  GameChooseMenu, GameSound;

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
  //Resources.LoadFromFiles; // cannot search recursively in Android assets
  Resources.AddFromFile(ApplicationData('creatures/arrow/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/werewolf/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/spider_queen/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/web/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/barrel/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/spider/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/ball_missile/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/ghost/resource.xml'));
  Resources.AddFromFile(ApplicationData('creatures/alien/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/sword/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/bow/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/key/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/life_potion/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/red_key/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/scroll/resource.xml'));
  Resources.AddFromFile(ApplicationData('items/quiver/resource.xml'));
  ItemsResourcesInit;
  CreaturesResourcesInit;
  //Levels.LoadFromFiles; // cannot search recursively in Android assets
  Levels.AddFromFile(ApplicationData('levels/doom/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/castle_hall/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/gate_background/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/fountain_with_water/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/hello_world/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/tower/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/gate/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/fountain/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/hello_world_shadow/level.xml'));
  Levels.AddFromFile(ApplicationData('levels/cages/level.xml'));
  Levels.SortByNumber;
  Levels.LoadFromConfig(UserConfig);

  { init sound. This displays progress of "Loading sounds". }
  SoundEngine.ALContextOpen;

  { create states }
  StateStartMenu := TStateStartMenu.Create(Application);
  StateCredits := TStateCredits.Create(Application);
  StateDebugMenu := TStateDebugMenu.Create(Application);
  StateControlsMenu := TStateControlsMenu.Create(Application);
  StateGameMenu := TStateGameMenu.Create(Application);
  StatePlay:= TStatePlay.Create(Application);
  StateChooseMenu := TStateChooseMenu.Create(Application);

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

  InitializeLog;

  Application.OnInitialize := @ApplicationInitialize;
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { for state credits (scrolling text animation), for state game }
  Window.AutoRedisplay := true;
end.
