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

{ Everything related to playing sound in "The Castle".
  In particular, TCastleSoundEngine descendant from TXmlSoundEngine. }
unit GameSound;

interface

uses Classes, VectorMath, SysUtils, XmlSoundEngine;

var
  { Castle sound types. Will be initialized in initialization of this
    unit (actually, along with creating TCastleSoundEngine and filling
    it's SoundNames property). You should not modify them afterwards. }

  { Player sounds.
    @groupBegin }
  stPlayerSuddenPain,
  stPlayerPotionDrink,
  stPlayerCastFlyingSpell,
  stPlayerPickItem,
  stPlayerDropItem,
  stPlayerDies,
  stPlayerSwimmingChange,
  stPlayerSwimming,
  stPlayerDrowning,
  stPlayerFalledDown,
  stPlayerFootstepsConcrete,
  stPlayerFootstepsGrass,
  stPlayerInteractFailed,
  stPlayerLavaPain,
  stPlayerFootstepsWatery,
  { @groupEnd }

  { Items sounds.
    @groupBegin }
  stSwordEquipping,
  stSwordAttackStart,
  stArrowFired,
  stArrowHit,
  stKeyUse,
  stKeyDoorUse,
  stBowAttackStart,
  stBowEquipping,
  { @groupEnd }

  { Levels sounds.
    @groupBegin }
  stGateMusic,
  stCastleHallSymbolMoving,
  stCastleHallMusic,
  stCagesMusic,
  stThunder,
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
  stWerewolfActualAttackHit,
  stWerewolfHowling,
  stBallMissileFired,
  stBallMissileExplode,
  stBallMissileIdle,
  stSpiderActualAttackHit,
  stSpiderAppears,
  stSpiderQueenActualAttackHit,
  stThrownWebFired,
  stThrownWebHit,
  stThrownWebIdle,
  { @groupEnd }

  { Others.
    @groupBegin }
  stIntroMusic,
  stMenuCurrentItemChanged,
  stMenuClick,
  stSaveScreen,
  stGameWinMusic
  { @groupEnd }
    :TSoundType;

type
  TCastleSoundEngine = class(TXmlSoundEngine)
  public
    constructor Create;
    destructor Destroy; override;
  end;

function SoundEngine: TCastleSoundEngine;

implementation

uses CastleGameConfig, ALSoundEngine;

constructor TCastleSoundEngine.Create;
begin
  inherited;

  LoadFromConfig(ConfigFile);

  ReadSounds;

  stPlayerSuddenPain           := SoundFromName('player_sudden_pain');
  stPlayerPotionDrink          := SoundFromName('player_potion_drink');
  stPlayerCastFlyingSpell      := SoundFromName('player_cast_flying_spell');
  stPlayerPickItem             := SoundFromName('player_pick_item');
  stPlayerDropItem             := SoundFromName('player_drop_item');
  stPlayerDies                 := SoundFromName('player_dies');
  stPlayerSwimmingChange       := SoundFromName('player_swimming_change');
  stPlayerSwimming             := SoundFromName('player_swimming');
  stPlayerDrowning             := SoundFromName('player_drowning');
  stPlayerFalledDown           := SoundFromName('player_falled_down');
  stPlayerFootstepsConcrete    := SoundFromName('player_footsteps_concrete');
  stPlayerFootstepsGrass       := SoundFromName('player_footsteps_grass');
  stPlayerInteractFailed       := SoundFromName('player_interact_failed');
  stPlayerLavaPain             := SoundFromName('player_lava_pain');
  stPlayerFootstepsWatery      := SoundFromName('player_footsteps_watery');
  stSwordEquipping             := SoundFromName('sword_equipping');
  stSwordAttackStart           := SoundFromName('sword_attack_start');
  stArrowFired                 := SoundFromName('arrow_fired');
  stArrowHit                   := SoundFromName('arrow_hit');
  stKeyUse                     := SoundFromName('key_use');
  stKeyDoorUse                 := SoundFromName('key_door_use');
  stBowAttackStart             := SoundFromName('bow_equipping');
  stBowEquipping               := SoundFromName('bow_attack_start');
  stGateMusic                  := SoundFromName('gate_music');
  stCastleHallSymbolMoving     := SoundFromName('castle_hall_symbol_moving');
  stCastleHallMusic            := SoundFromName('castle_hall_music');
  stCagesMusic                 := SoundFromName('cages_music_with_rain');
  stThunder                    := SoundFromName('thunder');
  stStairsBlockerDestroyed     := SoundFromName('stairs_blocker_destroyed');
  stTeleport                   := SoundFromName('teleport');
  stSacrilegeAmbush            := SoundFromName('sacrilege_ambush');
  stEvilLaugh                  := SoundFromName('evil_laugh');
  stDoomE1M1Music              := SoundFromName('doom_e1m1');
  stDoorOpen                   := SoundFromName('door_open');
  stDoorClose                  := SoundFromName('door_close');
  stElevator                   := SoundFromName('elevator');
  stDoomExitButton             := SoundFromName('doom_exit_button');
  stCreak                      := SoundFromName('creak');
  stWerewolfActualAttackHit    := SoundFromName('werewolf_actual_attack_hit');
  stWerewolfHowling            := SoundFromName('werewolf_howling');
  stBallMissileFired           := SoundFromName('ball_missile_fired');
  stBallMissileExplode         := SoundFromName('ball_missile_explode');
  stBallMissileIdle            := SoundFromName('ball_missile_idle');
  stSpiderActualAttackHit      := SoundFromName('spider_actual_attack_hit');
  stSpiderAppears              := SoundFromName('spider_appears');
  stSpiderQueenActualAttackHit := SoundFromName('spider_queen_actual_attack_hit');
  stThrownWebFired             := SoundFromName('thrown_web_fired');
  stThrownWebHit               := SoundFromName('thrown_web_hit');
  stThrownWebIdle              := SoundFromName('thrown_web_idle');
  stIntroMusic                 := SoundFromName('intro_music');
  stMenuCurrentItemChanged     := SoundFromName('menu_current_item_changed');
  stMenuClick                  := SoundFromName('menu_current_item_selected');
  stSaveScreen                 := SoundFromName('save_screen');
  stGameWinMusic               := SoundFromName('game_win_music');
end;

destructor TCastleSoundEngine.Destroy;
begin
  if ConfigFile <> nil then
    SaveToConfig(ConfigFile);

  inherited;
end;

function SoundEngine: TCastleSoundEngine;
begin
  Result := ALSoundEngine.SoundEngine as TCastleSoundEngine;
end;

{ initialization ------------------------------------------------------------- }

initialization
  ALSoundEngine.SoundEngine := TCastleSoundEngine.Create;
  { These were chosen experimentally for castle }
  SoundEngine.DistanceModel := dmInverseDistanceClamped; //< OpenAL default
  SoundEngine.DefaultRolloffFactor := 0.1;
  SoundEngine.DefaultReferenceDistance := 2.0;
end.
