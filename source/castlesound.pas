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

{ Everything related to playing sound in "The Castle".
  In particular, TCastleSoundEngine descendant from TGameSoundEngine.

  Note that this unit saves/restores ALCDevice value to/from config file. }
unit CastleSound;

interface

uses Classes, VectorMath, ALSourceAllocator, SysUtils, GameSoundEngine;

const
  LevelEventSoundImportance      = 100000;
  PlayerSoundImportance          = 10000;
  DefaultCreatureSoundImportance = 1000;
  MinorNonSpatialSoundImportance = 100;

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
  stCreatureFalledDown,
  stAlienSuddenPain,
  stAlienDying,
  stWerewolfSuddenPain,
  stWerewolfAttackStart,
  stWerewolfActualAttackHit,
  stWerewolfHowling,
  stWerewolfDying,
  stBallMissileFired,
  stBallMissileExplode,
  stBallMissileIdle,
  stGhostSuddenPain,
  stGhostAttackStart,
  stGhostDying,
  stSpiderActualAttackHit,
  stSpiderSuddenPain,
  stSpiderAttackStart,
  stSpiderDying,
  stSpiderAppears,
  stSpiderQueenActualAttackHit,
  stSpiderQueenSuddenPain,
  stSpiderQueenAttackStart,
  stSpiderQueenDying,
  stThrownWebFired,
  stThrownWebHit,
  stThrownWebIdle,
  { @groupEnd }

  { Others.
    @groupBegin }
  stIntroMusic,
  stMenuCurrentItemChanged,
  stMenuCurrentItemSelected,
  stSaveScreen,
  stGameWinMusic
  { @groupEnd }
    :TSoundType;

type
  TCastleSoundEngine = class(TGameSoundEngine)
  public
    constructor Create;
    destructor Destroy; override;
    procedure ALContextInit(WasParam_NoSound: boolean); override;
  end;

var
  SoundEngine: TCastleSoundEngine;

{ This initializes SoundEngine (makes it non-nil, if it's currently @nil).

  While this is called from initialization of this unit, there may
  happen circumstances when you want to call this explicitly.
  Namely, if you want to use SoundEngine from initialization of some
  other unit, and it happens that this initialization is before
  initialization of CastleSound. We have some non-trivial unit
  dependencies and loops here, so it happens. }
{ Not needed temporarily. }
{ procedure InitializeSoundEngine; }

implementation

uses CastleLog, CastleConfig, ALUtils;

constructor TCastleSoundEngine.Create;

  procedure SoundNameType(const Name: string; var Value: TSoundType);
  begin
    SoundNames.Append(Name);
    Value := SoundFromName(Name);
    Assert(Integer(Value) = SoundNames.Count - 1);
  end;

begin
  inherited;

  SoundNameType('player_sudden_pain',             stPlayerSuddenPain);
  SoundNameType('player_potion_drink',            stPlayerPotionDrink);
  SoundNameType('player_cast_flying_spell',       stPlayerCastFlyingSpell);
  SoundNameType('player_pick_item',               stPlayerPickItem);
  SoundNameType('player_drop_item',               stPlayerDropItem);
  SoundNameType('player_dies',                    stPlayerDies);
  SoundNameType('player_swimming_change',         stPlayerSwimmingChange);
  SoundNameType('player_swimming',                stPlayerSwimming);
  SoundNameType('player_drowning',                stPlayerDrowning);
  SoundNameType('player_falled_down',             stPlayerFalledDown);
  SoundNameType('player_footsteps_concrete',      stPlayerFootstepsConcrete);
  SoundNameType('player_footsteps_grass',         stPlayerFootstepsGrass);
  SoundNameType('player_interact_failed',         stPlayerInteractFailed);
  SoundNameType('player_lava_pain',               stPlayerLavaPain);
  SoundNameType('player_footsteps_watery',        stPlayerFootstepsWatery);
  SoundNameType('sword_equipping',                stSwordEquipping);
  SoundNameType('sword_attack_start',             stSwordAttackStart);
  SoundNameType('arrow_fired',                    stArrowFired);
  SoundNameType('arrow_hit',                      stArrowHit);
  SoundNameType('key_use',                        stKeyUse);
  SoundNameType('key_door_use',                   stKeyDoorUse);
  SoundNameType('bow_equipping',                  stBowAttackStart);
  SoundNameType('bow_attack_start',               stBowEquipping);
  SoundNameType('gate_music',                     stGateMusic);
  SoundNameType('castle_hall_symbol_moving',      stCastleHallSymbolMoving);
  SoundNameType('castle_hall_music',              stCastleHallMusic);
  SoundNameType('cages_music_with_rain',          stCagesMusic);
  SoundNameType('thunder',                        stThunder);
  SoundNameType('stairs_blocker_destroyed',       stStairsBlockerDestroyed);
  SoundNameType('teleport',                       stTeleport);
  SoundNameType('sacrilege_ambush',               stSacrilegeAmbush);
  SoundNameType('evil_laugh',                     stEvilLaugh);
  SoundNameType('doom_e1m1',                      stDoomE1M1Music);
  SoundNameType('door_open',                      stDoorOpen);
  SoundNameType('door_close',                     stDoorClose);
  SoundNameType('elevator',                       stElevator);
  SoundNameType('doom_exit_button',               stDoomExitButton);
  SoundNameType('creak',                          stCreak);
  SoundNameType('creature_falled_down',           stCreatureFalledDown);
  SoundNameType('alien_sudden_pain',              stAlienSuddenPain);
  SoundNameType('alien_dying',                    stAlienDying);
  SoundNameType('werewolf_sudden_pain',           stWerewolfSuddenPain);
  SoundNameType('werewolf_attack_start',          stWerewolfAttackStart);
  SoundNameType('werewolf_actual_attack_hit',     stWerewolfActualAttackHit);
  SoundNameType('werewolf_howling',               stWerewolfHowling);
  SoundNameType('werewolf_dying',                 stWerewolfDying);
  SoundNameType('ball_missile_fired',             stBallMissileFired);
  SoundNameType('ball_missile_explode',           stBallMissileExplode);
  SoundNameType('ball_missile_idle',              stBallMissileIdle);
  SoundNameType('ghost_sudden_pain',              stGhostSuddenPain);
  SoundNameType('ghost_attack_start',             stGhostAttackStart);
  SoundNameType('ghost_dying',                    stGhostDying);
  SoundNameType('spider_actual_attack_hit',       stSpiderActualAttackHit);
  SoundNameType('spider_sudden_pain',             stSpiderSuddenPain);
  SoundNameType('spider_attack_start',            stSpiderAttackStart);
  SoundNameType('spider_dying',                   stSpiderDying);
  SoundNameType('spider_appears',                 stSpiderAppears);
  SoundNameType('spider_queen_actual_attack_hit', stSpiderQueenActualAttackHit);
  SoundNameType('spider_queen_sudden_pain',       stSpiderQueenSuddenPain);
  SoundNameType('spider_queen_attack_start',      stSpiderQueenAttackStart);
  SoundNameType('spider_queen_dying',             stSpiderQueenDying);
  SoundNameType('thrown_web_fired',               stThrownWebFired);
  SoundNameType('thrown_web_hit',                 stThrownWebHit);
  SoundNameType('thrown_web_idle',                stThrownWebIdle);
  SoundNameType('intro_music',                    stIntroMusic);
  SoundNameType('menu_current_item_changed',      stMenuCurrentItemChanged);
  SoundNameType('menu_current_item_selected',     stMenuCurrentItemSelected);
  SoundNameType('save_screen',                    stSaveScreen);
  SoundNameType('game_win_music',                 stGameWinMusic);

  AddSoundImportanceName('level_event', LevelEventSoundImportance);
  AddSoundImportanceName('player', PlayerSoundImportance);
  AddSoundImportanceName('default_creature', DefaultCreatureSoundImportance);
  AddSoundImportanceName('minor_non_spatial', MinorNonSpatialSoundImportance);

  SoundVolume := ConfigFile.GetFloat('sound/volume',
    DefaultSoundVolume);
  MusicPlayer.MusicVolume := ConfigFile.GetFloat('sound/music/volume',
    DefaultMusicVolume);
  ALMinAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/min', DefaultALMinAllocatedSources);
  ALMaxAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/max', DefaultALMaxAllocatedSources);

  ALCDevice := ConfigFile.GetValue('sound/device', BestALCDevice);
end;

destructor TCastleSoundEngine.Destroy;
begin
  if ConfigFile <> nil then
  begin
    ConfigFile.SetDeleteFloat('sound/volume',
      SoundVolume, DefaultSoundVolume);
    if MusicPlayer <> nil then
      ConfigFile.SetDeleteFloat('sound/music/volume',
        MusicPlayer.MusicVolume, DefaultMusicVolume);
    ConfigFile.SetDeleteValue('sound/allocated_sources/min',
      ALMinAllocatedSources, DefaultALMinAllocatedSources);
    ConfigFile.SetDeleteValue('sound/allocated_sources/max',
      ALMaxAllocatedSources, DefaultALMaxAllocatedSources);
    ConfigFile.SetDeleteValue('sound/device', ALCDevice, BestALCDevice);
  end;
  inherited;
end;

procedure TCastleSoundEngine.ALContextInit(WasParam_NoSound: boolean);
begin
  inherited;
  if WasParam_DebugLog then
    WritelnLog(ltSoundInitialization, SoundInitializationReport);
end;

{ initialization ------------------------------------------------------------- }

procedure InitializeSoundEngine;
begin
  if SoundEngine = nil then
    SoundEngine := TCastleSoundEngine.Create;
end;

initialization
  InitializeSoundEngine;
finalization
  FreeAndNil(SoundEngine);
end.
