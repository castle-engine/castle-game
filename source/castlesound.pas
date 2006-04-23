{
  Copyright 2006 Michalis Kamburelis.

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

{ For global property. In the future whole castle (and units/)
  code will be in objfpc mode. }
{$mode objfpc}

{ }
unit CastleSound;

interface

uses VectorMath, ALSourceAllocator;

var
  SoundInitializationReport: string;

const
  MaxSoundImportance = MaxInt;

type
  TSoundType = (
    { Special sound type that indicates that there is actually none sound.
      @link(Sound) and @link(Sound3d) will do nothing when called with
      this sound type. }
    stNone,

    { Player sounds.
      @groupBegin }
    stPlayerSuddenPain,
    stPlayerPotionDrink,
    stPlayerCastFlyingSpell,
    stPlayerPickItem,
    stPlayerDropItem,
    stPlayerDies,
    stPlayerSwimmingBegin,
    stPlayerSwimmingEnd,
    stPlayerDrowning,
    { @groupEnd }

    { Items sounds.
      @groupBegin }
    stSwordEquipping,
    stSwordAttackStart,
    { @groupEnd }

    { Level objects sounds.
      @groupBegin }
    stCastleHallSymbolMoving,
    { @groupEnd }

    { Creatures sounds.
      @groupBegin }
    stAlienSuddenPain,
    stWerewolfSuddenPain,
    stWerewolfAttackStart,
    stWerewolfActualAttackHit,
    stWerewolfHowling,
    stBallMissileFired,
    stBallMissileExplode,
    stGhostSuddenPain,
    stGhostAttackStart,
    { @groupEnd }

    { Others.
      @groupBegin }
    stMenuCurrentItemChanged,
    stMenuCurrentItemSelected,
    stSaveScreen
    { @groupEnd });

{ Call this always to initialize OpenAL and OpenAL context,
  and load sound files. This sets SoundInitializationReport
  and ALActive.

  You can set ALCDevice before calling this.
  Note that this unit saves/restores ALCDevice value to/from config file. }
procedure ALContextInit(WasParam_NoSound: boolean);

{ This will call RefreshUsed on internal ALSourceAllocator,
  see TALSourceAllocator.RefreshUsed for info.
  It's silently ignored when not ALActive. }
procedure ALRefreshUsedSources;

{ Call this always to release OpenAL things.
  This is ignored if not ALActive. }
procedure ALContextClose;

{ Play given sound. This should be used to play sounds
  that are not spatial actually, i.e. have no place in 3D space.

  Returns used TALAllocatedSource (or nil if none was available).
  You don't have to do anything with this returned TALAllocatedSource.

  @noAutoLinkHere }
function Sound(SoundType: TSoundType;
  const Looping: boolean = false): TALAllocatedSource;

{ Play given sound at appropriate position in 3D space.

  Returns used TALAllocatedSource (or nil if none was available).
  You don't have to do anything with this returned TALAllocatedSource.

  @noAutoLinkHere }
function Sound3d(SoundType: TSoundType;
  const Position: TVector3Single;
  const Looping: boolean = false): TALAllocatedSource; overload;

{ }
function GetEffectsVolume: Single;
procedure SetEffectsVolume(const Value: Single);

{ Sound effects volume. This must always be within 0..1 range.
  0.0 means that there are no effects (this case should be optimized). }
property EffectsVolume: Single read GetEffectsVolume write SetEffectsVolume;

function GetMusicVolume: Single;
procedure SetMusicVolume(const Value: Single);

{ Music volume. This must always be within 0..1 range.
  0.0 means that there is no music (this case should be optimized).}
property MusicVolume: Single read GetMusicVolume write SetMusicVolume;

{ When changing Min/MaxAllocatedSources, remember to always keep
  MinAllocatedSources <= MaxAllocatedSources. }

{ }
function GetALMinAllocatedSources: Cardinal;
procedure SetALMinAllocatedSources(const Value: Cardinal);

const
  DefaultALMinAllocatedSources = 16;

property ALMinAllocatedSources: Cardinal
  read GetALMinAllocatedSources write SetALMinAllocatedSources;

function GetALMaxAllocatedSources: Cardinal;
procedure SetALMaxAllocatedSources(const Value: Cardinal);

const
  DefaultALMaxAllocatedSources = 32;

property ALMaxAllocatedSources: Cardinal
  read GetALMaxAllocatedSources write SetALMaxAllocatedSources;

type
  TSoundInfo = record
    { '' means that this sound is not implemented and will have
      no OpenAL buffer associated with it. }
    FileName: string;

    { XxxGain are mapped directly on respective OpenAL source properties.
      Note that Gain and MaxGain > 1 are allowed (because OpenAL allows them),
      although OpenAL may clip them for the resulting sound (after all
      calculations taking into account 3d position will be done). }
    Gain, MinGain, MaxGain: Single;

    DefaultImportance: Cardinal;
  end;

var
  { Properties of sounds.

    For the actual game, as used by end-user, SoundInfos is a constant,
    moreover it's internal to this unit (so you shouldn't even read
    this).

    However, for the sake of debugging/testing the game,
    you can read and change some things in SoundInfos:
    right now, you can reliably change everything except FileName.
    Note that the changes will only be reflected in new sounds,
    not in currently played sounds. }
  SoundInfos: array[TSoundType] of TSoundInfo =
  { TODO: fill all sounds below. }
  ( ( FileName: '';
      Gain: 0; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_sudden_pain.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_potion_drink.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_cast_flying_spell.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_pick_item.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_drop_item.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_dies.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_swimming_begin.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_swimming_end.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_drowning.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { sword_equipping.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { sword_attack_start.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { castle_hall_symbol_moving.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { alien_sudden_pain.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_sudden_pain.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_attack_start.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_actual_attack_hit.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: 'werewolf_howling.wav';
      Gain: 1; MinGain: 0.8; MaxGain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: '' { ball_missile_fired.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ball_missile_explode.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ghost_sudden_pain.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ghost_attack_start.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: 'menu_current_item_changed.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: '' { 'menu_current_item_selected.wav' };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; ),
    ( FileName: '' { save_screen.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: 0; )
  );

implementation

uses SysUtils, CastleConfig, ProgressUnit, OpenAL, ALUtils, KambiUtils,
  KambiFilesUtils;

var
  { Values on this array are useful only when ALContextInited
    and only for sounds with SoundInfos[].FileName <> ''. }
  SoundBuffers: array[TSoundType] of TALuint;

  { When SourceAllocator <> nil, these correspond to it's properties. }
  FALMinAllocatedSources: Cardinal;
  FALMaxAllocatedSources: Cardinal;

  SourceAllocator: TALSourceAllocator;

procedure ALContextInit(WasParam_NoSound: boolean);
var
  ST: TSoundType;
begin
  Assert(not ALActive);

  if WasParam_NoSound then
    SoundInitializationReport :=
      'Sound disabled by --no-sound command-line option' else
  if not TryBeginAL(false) then
    SoundInitializationReport :=
      'OpenAL initialization failed : ' +ALActivationErrorMessage +nl+
      'SOUND IS DISABLED' else
  begin
    SoundInitializationReport :=
      'OpenAL initialized, sound enabled';

    try
      SourceAllocator := TALSourceAllocator.Create(
        FALMinAllocatedSources, FALMaxAllocatedSources);

      alListenerf(AL_GAIN, EffectsVolume);

      Progress.Init(Ord(High(TSoundType)) + 1, 'Loading sounds');
      try
        for ST := Low(TSoundType) to High(TSoundType) do
        begin
          if SoundInfos[ST].FileName <> '' then
          begin
            SoundBuffers[ST] := TALSoundWAV.alCreateBufferDataFromFile(
              ProgramDataPath + PathDelim + 'data' + PathDelim +
              'sounds' + PathDelim + SoundInfos[ST].FileName);
          end;
          Progress.Step;
        end;
      finally Progress.Fini; end;
    except
      { If loading sounds above will fail, we have to finish already initialized
        things here before reraising exception. }
      FreeAndNil(SourceAllocator);
      EndAL;
      raise;
    end;
  end;
end;

procedure ALContextClose;
var
  ST: TSoundType;
begin
  if ALActive then
  begin
    FreeAndNil(SourceAllocator);

    for ST := Low(TSoundType) to High(TSoundType) do
      if SoundInfos[ST].FileName <> '' then
        alDeleteBuffers(1, @SoundBuffers[ST]);

    EndAL;
  end;
end;

procedure ALRefreshUsedSources;
begin
  if SourceAllocator <> nil then
    SourceAllocator.RefreshUsed;
end;

{ Set common properties for spatialized and non-spatialized
  sound effects. }
procedure alCommonSourceSetup(ALSource: TALuint; SoundType: TSoundType;
  const Looping: boolean);
begin
  alSourcei(ALSource, AL_BUFFER, SoundBuffers[SoundType]);
  alSourcei(ALSource, AL_LOOPING, BoolToAL[Looping]);
  alSourcef(ALSource, AL_GAIN, SoundInfos[SoundType].Gain);
  alSourcef(ALSource, AL_MIN_GAIN, SoundInfos[SoundType].MinGain);
  alSourcef(ALSource, AL_MAX_GAIN, SoundInfos[SoundType].MaxGain);
end;

function Sound(SoundType: TSoundType;
  const Looping: boolean): TALAllocatedSource;
begin
  Result := nil;

  if ALActive and (SoundInfos[SoundType].FileName <> '') then
  begin
    Result := SourceAllocator.AllocateSource(
      SoundInfos[SoundType].DefaultImportance);
    if Result <> nil then
    begin
      alCommonSourceSetup(Result.ALSource, SoundType, Looping);

      alSourcei(Result.ALSource, AL_SOURCE_RELATIVE, AL_TRUE);
      { Windows OpenAL doesn't work correctly when below is exactly
        (0, 0, 0) --- see /win/docs/audio/OpenAL/bug_relative_0/
        TODO: check does this bug still exist ?
        Correct lets_take_a_walk, test_al_sound_allocator }
      alSourceVector3f(Result.ALSource, AL_POSITION, Vector3Single(0, 0, 0.1));

      alSourcePlay(Result.ALSource);
    end;
  end;
end;

function Sound3d(SoundType: TSoundType;
  const Position: TVector3Single;
  const Looping: boolean): TALAllocatedSource;
begin
  Result := nil;

  if ALActive and (SoundInfos[SoundType].FileName <> '') then
  begin
    Result := SourceAllocator.AllocateSource(
      SoundInfos[SoundType].DefaultImportance);
    if Result <> nil then
    begin
      alCommonSourceSetup(Result.ALSource, SoundType, Looping);

      alSourcei(Result.ALSource, AL_SOURCE_RELATIVE, AL_FALSE);
      alSourceVector3f(Result.ALSource, AL_POSITION, Position);

      alSourcePlay(Result.ALSource);
    end;
  end;
end;

const
  DefaultEffectsVolume = 0.5;

var
  FEffectsVolume: Single;

function GetEffectsVolume: Single;
begin
  Result := FEffectsVolume;
end;

procedure SetEffectsVolume(const Value: Single);
begin
  if Value <> FEffectsVolume then
  begin
    FEffectsVolume := Value;
    if ALActive then
      alListenerf(AL_GAIN, EffectsVolume);
  end;
end;

const
  DefaultMusicVolume = 0.5;

var
  FMusicVolume: Single;

function GetMusicVolume: Single;
begin
  Result := FMusicVolume;
end;

procedure SetMusicVolume(const Value: Single);
begin
  if Value <> FMusicVolume then
  begin
    FMusicVolume := Value;
    { TODO }
  end;
end;

function GetALMinAllocatedSources: Cardinal;
begin
  Result := FALMinAllocatedSources;
end;

procedure SetALMinAllocatedSources(const Value: Cardinal);
begin
  if Value <> FALMinAllocatedSources then
  begin
    FALMinAllocatedSources := Value;
    if SourceAllocator <> nil then
      SourceAllocator.MinAllocatedSources := FALMinAllocatedSources;
  end;
end;

function GetALMaxAllocatedSources: Cardinal;
begin
  Result := FALMaxAllocatedSources;
end;

procedure SetALMaxAllocatedSources(const Value: Cardinal);
begin
  if Value <> FALMaxAllocatedSources then
  begin
    FALMaxAllocatedSources := Value;
    if SourceAllocator <> nil then
      SourceAllocator.MaxAllocatedSources := FALMaxAllocatedSources;
  end;
end;

initialization
  FEffectsVolume := ConfigFile.GetValue('sound/effects/volume', DefaultEffectsVolume);
  FMusicVolume   := ConfigFile.GetValue('sound/music/volume', DefaultMusicVolume);
  FALMinAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/min', DefaultALMinAllocatedSources);
  FALMaxAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/max', DefaultALMaxAllocatedSources);
  ALCDevice := ConfigFile.GetValue('sound/device', BestALCDevice);
finalization
  ConfigFile.SetDeleteValue('sound/effects/volume', EffectsVolume, DefaultEffectsVolume);
  ConfigFile.SetDeleteValue('sound/music/volume', MusicVolume, DefaultMusicVolume);
  ConfigFile.SetDeleteValue('sound/allocated_sources/min',
    FALMinAllocatedSources, DefaultALMinAllocatedSources);
  ConfigFile.SetDeleteValue('sound/allocated_sources/max',
    FALMaxAllocatedSources, DefaultALMaxAllocatedSources);
  ConfigFile.SetDeleteValue('sound/device', ALCDevice, BestALCDevice);
end.