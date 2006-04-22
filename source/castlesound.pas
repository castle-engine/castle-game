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

uses VectorMath;

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
    stSaveScreen
    { @groupEnd });

{ Call this always to initialize OpenAL and OpenAL context,
  and load sounds. This sets SoundInitializationReport
  and ALActive. }
procedure ALContextInit(WasParam_NoSound: boolean);

{ Call this always to release OpenAL things.
  This is ignored if not ALActive. }
procedure ALContextClose;

{ Play given sound. This should be used to play sounds
  that are not spatial actually, i.e. have no place in 3D space.
  @noAutoLinkHere }
procedure Sound(SoundType: TSoundType);

{ Play given sound at appropriate position in 3D space.
  @noAutoLinkHere }
procedure Sound3d(SoundType: TSoundType; const Position: TVector3Single);

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

{ When changing Min/MaxAllocatedSounds, remember to always keep
  MinAllocatedSounds <= MaxAllocatedSounds. }

{ }
function GetALMinAllocatedSounds: Cardinal;
procedure SetALMinAllocatedSounds(const Value: Cardinal);

const
  DefaultALMinAllocatedSounds = 16;

property ALMinAllocatedSounds: Cardinal
  read GetALMinAllocatedSounds write SetALMinAllocatedSounds;

function GetALMaxAllocatedSounds: Cardinal;
procedure SetALMaxAllocatedSounds(const Value: Cardinal);

const
  DefaultALMaxAllocatedSounds = 32;

property ALMaxAllocatedSounds: Cardinal
  read GetALMaxAllocatedSounds write SetALMaxAllocatedSounds;

implementation

uses SysUtils, CastleConfig, ProgressUnit, OpenAL, ALUtils, KambiUtils,
  KambiFilesUtils, ALSoundAllocator;

type
  TSoundInfo = record
    { '' means that this sound is not implemented and will have
      no OpenAL buffer associated with it. }
    FileName: string;
    Gain: Single;
    DefaultImportance: Cardinal;
  end;

const
  SoundInfos: array[TSoundType] of TSoundInfo =
  ( ( FileName: ''; Gain: 0; DefaultImportance: 0; ),
    ( FileName: '' { player_sudden_pain.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_potion_drink.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_cast_flying_spell.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_pick_item.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_drop_item.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_dies.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_swimming_begin.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_swimming_end.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { player_drowning.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { sword_equipping.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { sword_attack_start.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { castle_hall_symbol_moving.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { alien_sudden_pain.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_sudden_pain.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_attack_start.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_actual_attack_hit.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { werewolf_howling.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ball_missile_fired.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ball_missile_explode.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ghost_sudden_pain.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: '' { ghost_attack_start.wav }; Gain: 1; DefaultImportance: 0; ),
    ( FileName: 'menu_current_item_changed.wav'; Gain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: '' { save_screen.wav }; Gain: 1; DefaultImportance: 0; )
  );

var
  { Values on this array are useful only when ALContextInited
    and only for sounds with SoundInfos[].FileName <> ''. }
  SoundBuffers: array[TSoundType] of TALuint;

  { When SoundAllocator <> nil, these correspond to it's properties. }
  FALMinAllocatedSounds: Cardinal;
  FALMaxAllocatedSounds: Cardinal;

  SoundAllocator: TALSoundAllocator;

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
      SoundAllocator := TALSoundAllocator.Create(
        FALMinAllocatedSounds, FALMaxAllocatedSounds);

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
      FreeAndNil(SoundAllocator);
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
    FreeAndNil(SoundAllocator);

    for ST := Low(TSoundType) to High(TSoundType) do
      if SoundInfos[ST].FileName <> '' then
        alDeleteBuffers(1, @SoundBuffers[ST]);

    EndAL;
  end;
end;

procedure Sound(SoundType: TSoundType);
var
  NewSound: TALAllocatedSound;
begin
  if ALActive and (SoundInfos[SoundType].FileName <> '') then
  begin
    NewSound := SoundAllocator.AllocateSound(
      SoundInfos[SoundType].DefaultImportance);
    if NewSound <> nil then
    begin
      alSourcei(NewSound.ALSound, AL_BUFFER, SoundBuffers[SoundType]);
      alSourcef(NewSound.ALSound, AL_GAIN, SoundInfos[SoundType].Gain);

      { AL_MIN_GAIN and AL_MAX_GAIN should also be set }

      alSourcei(NewSound.ALSound, AL_SOURCE_RELATIVE, AL_TRUE);
      { Windows OpenAL doesn't work correctly when below is exactly
        (0, 0, 0) --- see /win/docs/audio/OpenAL/bug_relative_0/

        TODO: check does this bug still exist ?
        Correct lets_take_a_walk, test_al_sound_allocator }
      alSourceVector3f(NewSound.ALSound, AL_POSITION, Vector3Single(0, 0, 0.1));

      alSourcePlay(NewSound.ALSound);
    end;
  end;
end;

procedure Sound3d(SoundType: TSoundType; const Position: TVector3Single);
begin
  if SoundType <> stNone then
  begin
    { TODO }
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
    { TODO }
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

function GetALMinAllocatedSounds: Cardinal;
begin
  Result := FALMinAllocatedSounds;
end;

procedure SetALMinAllocatedSounds(const Value: Cardinal);
begin
  if Value <> FALMinAllocatedSounds then
  begin
    FALMinAllocatedSounds := Value;
    if SoundAllocator <> nil then
      SoundAllocator.MinAllocatedSounds := FALMinAllocatedSounds;
  end;
end;

function GetALMaxAllocatedSounds: Cardinal;
begin
  Result := FALMaxAllocatedSounds;
end;

procedure SetALMaxAllocatedSounds(const Value: Cardinal);
begin
  if Value <> FALMaxAllocatedSounds then
  begin
    FALMaxAllocatedSounds := Value;
    if SoundAllocator <> nil then
      SoundAllocator.MaxAllocatedSounds := FALMaxAllocatedSounds;
  end;
end;

initialization
  FEffectsVolume := ConfigFile.GetValue('sound/effects/volume', DefaultEffectsVolume);
  FMusicVolume   := ConfigFile.GetValue('sound/music/volume', DefaultMusicVolume);
  FALMinAllocatedSounds := ConfigFile.GetValue(
    'sound/allocated_sounds/min', DefaultALMinAllocatedSounds);
  FALMaxAllocatedSounds := ConfigFile.GetValue(
    'sound/allocated_sounds/max', DefaultALMaxAllocatedSounds);
finalization
  ConfigFile.SetDeleteValue('sound/effects/volume', EffectsVolume, DefaultEffectsVolume);
  ConfigFile.SetDeleteValue('sound/music/volume', MusicVolume, DefaultMusicVolume);
  ConfigFile.SetDeleteValue('sound/allocated_sounds/min',
    FALMinAllocatedSounds, DefaultALMinAllocatedSounds);
  ConfigFile.SetDeleteValue('sound/allocated_sounds/max',
    FALMaxAllocatedSounds, DefaultALMaxAllocatedSounds);
end.