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
  { castle GameXxx units }
  GameWindow, GameStartMenu, GameHelp, CastleFilesUtils, GameControlsMenu,
  GameVideoOptions, GameInitialBackground, GameItems, GameDebugMenu,
  GameCreatures, GamePlay, GameGeneralMenu, GameCredits, GameGameMenu,
  GameChooseMenu;

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

  { load game data from XML files }
  MaterialProperties.URL := ApplicationData('textures/material_properties.xml');
  Resources.LoadFromFiles;
  ItemsResourcesInit;
  CreaturesResourcesInit;
  Levels.LoadFromFiles;
  Levels.LoadFromConfig(UserConfig);

  { init OpenAL (after setting StateInitial, because ALContextOpen
    wants to display progress of "Loading sounds") }
  StateInitial := TStateInitial.Create(Application);
  TUIState.Current := StateInitial;

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
