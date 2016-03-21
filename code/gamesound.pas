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

{ Sounds in "The Castle". }
unit GameSound;

interface

uses Classes, SysUtils, CastleSoundEngine;

var
  { Castle sound types. Will be initialized in initialization of this
    unit (actually, along with creating TCastleSoundEngine and filling
    it's SoundNames property). You should not modify them afterwards. }

  { Player sounds.
    @groupBegin }
  stPlayerPotionDrink,
  stPlayerCastFlyingSpell,
  { @groupEnd }

  { Items sounds.
    @groupBegin }
  stKeyUse,
  stKeyDoorUse,
  { @groupEnd }

  { Levels sounds.
    @groupBegin }
  stCastleHallSymbolMoving,
  stStairsBlockerDestroyed,
  stTeleport,
  stSacrilegeAmbush,
  stEvilLaugh,
  stDoomE1M1Music,
  stDoorOpen,
  stDoorClose,
  stElevator,
  stDoomExitButton,
  stCreak,
  { @groupEnd }

  { Creatures sounds.
    @groupBegin }
  stWerewolfHowling,
  stSpiderAppears,
  { @groupEnd }

  { Others.
    @groupBegin }
  stIntroMusic,
  stSaveScreen,
  stGameWinMusic
  { @groupEnd }
    :TSoundType;

procedure InitializeSound;

implementation

uses CastleFilesUtils;

procedure InitializeSound;
begin
  SoundEngine.RepositoryURL := ApplicationData('sounds/index.xml');

  stPlayerPotionDrink          := SoundEngine.SoundFromName('player_potion_drink');
  stPlayerCastFlyingSpell      := SoundEngine.SoundFromName('player_cast_flying_spell');
  stKeyUse                     := SoundEngine.SoundFromName('key_use');
  stKeyDoorUse                 := SoundEngine.SoundFromName('key_door_use');
  stCastleHallSymbolMoving     := SoundEngine.SoundFromName('castle_hall_symbol_moving');
  stStairsBlockerDestroyed     := SoundEngine.SoundFromName('stairs_blocker_destroyed');
  stTeleport                   := SoundEngine.SoundFromName('teleport');
  stSacrilegeAmbush            := SoundEngine.SoundFromName('sacrilege_ambush');
  stEvilLaugh                  := SoundEngine.SoundFromName('evil_laugh');
  stDoomE1M1Music              := SoundEngine.SoundFromName('doom_e1m1');
  stDoorOpen                   := SoundEngine.SoundFromName('door_open');
  stDoorClose                  := SoundEngine.SoundFromName('door_close');
  stElevator                   := SoundEngine.SoundFromName('elevator');
  stDoomExitButton             := SoundEngine.SoundFromName('doom_exit_button');
  stCreak                      := SoundEngine.SoundFromName('creak');
  stWerewolfHowling            := SoundEngine.SoundFromName('werewolf_howling');
  stSpiderAppears              := SoundEngine.SoundFromName('spider_appears');
  stIntroMusic                 := SoundEngine.SoundFromName('intro_music');
  stSaveScreen                 := SoundEngine.SoundFromName('save_screen');
  stGameWinMusic               := SoundEngine.SoundFromName('game_win_music');

  { These were chosen experimentally for castle }
  SoundEngine.DistanceModel := dmInverseDistanceClamped; //< OpenAL default
  SoundEngine.DefaultRolloffFactor := 0.1;
  SoundEngine.DefaultReferenceDistance := 2.0;
end;

end.
