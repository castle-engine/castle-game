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

uses Classes, VectorMath, ALSourceAllocator;

var
  SoundInitializationReport: string;

const
  MaxSoundImportance             = MaxInt;
  LevelEventSoundImportance      = 100000;
  PlayerSoundImportance          = 10000;
  DefaultCreatureSoundImportance = 1000;
  MinorNonSpatialSoundImportance = 100;

type
  TSoundType = (
    { Special sound type that indicates that there is actually none sound.
      @link(Sound) and @link(Sound3d) will do nothing when called with
      this sound type. }
    stNone,

    { Player sounds.
      @groupBegin }
    { Note for stPlayerSuddenPain:
      After trying many sounds (things that sound like someone saying "ouh",
      things that sound like some "thud" etc.), nothing seems to sound OK.

      There are various ways how you can get hurt, and many of
      them already have appropriate sounds --- e.g. on drowning you get
      stPlayerDrowning, on falling down you get stFalledDown. When being
      hit by the creature it's at least supposed that we'll create some
      sounds for particular creatures and their particular attacks
      (e.g. when hit by ball_missile, player gets ball_missile_explode sound
      already).

      So maybe it's not a good idea to create a general sound
      that will be played always when player is wound ? }
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
    { @groupEnd }

    { Items sounds.
      @groupBegin }
    stSwordEquipping,
    stSwordAttackStart,
    stArrowHit { TODO: not used for now. },
    stKeyUse { TODO: not used for now. },
    { @groupEnd }

    { Levels sounds.
      @groupBegin }
    stGateMusic,
    stCastleHallSymbolMoving,
    stCastleHallMusic,
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
    stGhostSuddenPain,
    stGhostAttackStart,
    stGhostDying,
    { @groupEnd }

    { Others.
      @groupBegin }
    stIntroMusic,
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

{ If ALActive, then will append some info about current OpenAL used. }
procedure AppendALInformation(S: TStrings);

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
function GetSoundVolume: Single;
procedure SetSoundVolume(const Value: Single);

{ Sound volume, affects all sounds (effects and music).
  This must always be within 0..1 range.
  0.0 means that there are no effects (this case should be optimized). }
property SoundVolume: Single read GetSoundVolume write SetSoundVolume;

function GetMusicVolume: Single;
procedure SetMusicVolume(const Value: Single);

{ Music volume. This must always be within 0..1 range.
  0.0 means that there is no music (this case should be optimized).}
property MusicVolume: Single read GetMusicVolume write SetMusicVolume;

type
  TMusicPlayer = class
  private
    FPlayedSound: TSoundType;
    procedure SetPlayedSound(const Value: TSoundType);

    { This is nil if we don't play music right now
      (because OpenAL is not initialized, or PlayedSound = stNone,
      or PlayerSound.FileName = '' (not implemented)). }
    FAllocatedSource: TALAllocatedSource;

    procedure AllocatedSourceUsingEnd(Sender: TALAllocatedSource);

    { Called by ALInitContext. You should check here if
      PlayedSound <> stNone and eventually initialize FAllocatedSource. }
    procedure AllocateSource;
  public
    destructor Destroy; override;

    { Currently played music.
      Set to stNone to stop playing music.
      Set to anything else to play that music.

      Changing value of this property (when both the old and new values
      are <> stNone and are different) restarts playing the music. }
    property PlayedSound: TSoundType read FPlayedSound write SetPlayedSound
      default stNone;
  end;

var
  { This is the only allowed instance of TMusicPlayer class,
    created and destroyed in this unit's init/fini. }
  MusicPlayer: TMusicPlayer;

{ When changing Min/MaxAllocatedSources, remember to always keep
  MinAllocatedSources <= MaxAllocatedSources. }

{ }
function GetALMinAllocatedSources: Cardinal;
procedure SetALMinAllocatedSources(const Value: Cardinal);

const
  DefaultALMinAllocatedSources = 4;

property ALMinAllocatedSources: Cardinal
  read GetALMinAllocatedSources write SetALMinAllocatedSources;

function GetALMaxAllocatedSources: Cardinal;
procedure SetALMaxAllocatedSources(const Value: Cardinal);

const
  DefaultALMaxAllocatedSources = 16;

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
      calculations taking into account 3d position will be done).

      When sound is used for MusicPlayer.PlayedSound:
      1. MinGain, MaxGain are ignored
      2. Gain is always multiplied by MusicVolume when setting AL_GAIN
         of the music source. }
    Gain, MinGain, MaxGain: Single;

    { Importance, as passed to TALSourceAllocator.
      This is ignored when sound is used for MusicPlayer.PlayedSound. }
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
      Gain: 0; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: '' { 'player_sudden_pain.wav' };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_potion_drink.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_cast_flying_spell.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_pick_item.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_drop_item.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_dies.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_swimming_change.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_swimming.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_drowning.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_falled_down.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_footsteps_concrete.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_footsteps_grass.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'player_interact_failed.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'sword_equipping.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: '' { sword_attack_start.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'arrow_hit.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'key_use.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: PlayerSoundImportance; ),
    ( FileName: 'gate_music.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: 'castle_hall_symbol_moving.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: LevelEventSoundImportance; ),
    ( FileName: 'castle_hall_music.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: 'creature_falled_down.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'alien_sudden_pain.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'alien_dying.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'werewolf_sudden_pain.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'werewolf_attack_start.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: '' { werewolf_actual_attack_hit.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'werewolf_howling.wav';
      Gain: 1; MinGain: 0.8; MaxGain: 1; DefaultImportance: LevelEventSoundImportance; ),
    ( FileName: 'werewolf_dying.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'ball_missile_fired.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'ball_missile_explode.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'ghost_sudden_pain.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'ghost_attack_start.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'ghost_dying.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: DefaultCreatureSoundImportance; ),
    ( FileName: 'intro_music.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MaxSoundImportance; ),
    ( FileName: 'menu_current_item_changed.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MinorNonSpatialSoundImportance; ),
    ( FileName: 'menu_current_item_selected.wav';
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MinorNonSpatialSoundImportance; ),
    ( FileName: '' { save_screen.wav };
      Gain: 1; MinGain: 0; MaxGain: 1; DefaultImportance: MinorNonSpatialSoundImportance; )
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

      alListenerf(AL_GAIN, SoundVolume);

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

      MusicPlayer.AllocateSource;

      CheckAL('initializing sounds (ALContextInit)');
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

    { EndAL may take a while on Unix OpenAL, so provide feedback
      for user here (otherwise he (she?) may think that program hanged. }
    Progress.Init(1, 'Closing sound device, please wait');
    try
      EndAL;
      Progress.Step;
    finally Progress.Fini; end;
  end;
end;

procedure ALRefreshUsedSources;
begin
  if SourceAllocator <> nil then
    SourceAllocator.RefreshUsed;
end;

{ Set common properties for spatialized and non-spatialized
  sound effects. If Spatial = true, you have to always set this sound's
  AL_POSITION after calling this. }
procedure alCommonSourceSetup(ALSource: TALuint;
  const Looping: boolean;
  const Spatial: boolean;
  const ALBuffer: TALuint; const Gain, MinGain, MaxGain: Single); overload;
begin
  alSourcei(ALSource, AL_BUFFER, ALBuffer);
  alSourcei(ALSource, AL_LOOPING, BoolToAL[Looping]);
  alSourcef(ALSource, AL_GAIN, Gain);
  alSourcef(ALSource, AL_MIN_GAIN, MinGain);
  alSourcef(ALSource, AL_MAX_GAIN, MaxGain);

  if Spatial then
  begin
    { Set attenuation by distance. }
    alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0.1);
    alSourcef(ALSource, AL_REFERENCE_DISTANCE, 2.0);

    alSourcei(ALSource, AL_SOURCE_RELATIVE, AL_FALSE);
  end else
  begin
    { No attenuation by distance. }
    alSourcef(ALSource, AL_ROLLOFF_FACTOR, 0);

    { Although AL_ROLLOFF_FACTOR := 0 turns off
      attenuation by distance, we still have to turn off
      any changes from player's orientation (so that the sound
      is not played on left or right side, but normally).
      That's why setting source position exactly on the player
      is needed here. }
    alSourcei(ALSource, AL_SOURCE_RELATIVE, AL_TRUE);
    alSourceVector3f(ALSource, AL_POSITION, Vector3Single(0, 0, 0));
  end;
end;

procedure alCommonSourceSetup(ALSource: TALuint; SoundType: TSoundType;
  const Looping: boolean;
  const Spatial: boolean); overload;
begin
  alCommonSourceSetup(ALSource, Looping, Spatial, SoundBuffers[SoundType],
    SoundInfos[SoundType].Gain,
    SoundInfos[SoundType].MinGain,
    SoundInfos[SoundType].MaxGain);
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
      alCommonSourceSetup(Result.ALSource, SoundType, Looping, false);
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
      alCommonSourceSetup(Result.ALSource, SoundType, Looping, true);
      alSourceVector3f(Result.ALSource, AL_POSITION, Position);
      alSourcePlay(Result.ALSource);
    end;
  end;
end;

const
  DefaultSoundVolume = 0.5;

var
  FSoundVolume: Single;

function GetSoundVolume: Single;
begin
  Result := FSoundVolume;
end;

procedure SetSoundVolume(const Value: Single);
begin
  if Value <> FSoundVolume then
  begin
    FSoundVolume := Value;
    if ALActive then
      alListenerf(AL_GAIN, SoundVolume);
  end;
end;

procedure AppendALInformation(S: TStrings);
begin
  if ALActive then
  begin
    S.Append('');
    S.Append('Version : ' + alGetString(AL_VERSION));
    S.Append('Renderer : ' + alGetString(AL_RENDERER));
    S.Append('Vendor : ' + alGetString(AL_VENDOR));
    S.Append('Extensions : ' + alGetString(AL_EXTENSIONS));
    S.Append('');
    S.Append(Format('Allocated OpenAL sources: %d (min %d, max %d)',
      [ SourceAllocator.AllocatedSources.Count,
        SourceAllocator.MinAllocatedSources,
        SourceAllocator.MaxAllocatedSources ]));
  end;
end;

{ TMusicPlayer --------------------------------------------------------------- }

destructor TMusicPlayer.Destroy;
begin
  if FAllocatedSource <> nil then
    FAllocatedSource.DoUsingEnd;
  inherited;
end;

procedure TMusicPlayer.AllocateSource;
begin
  if ALActive and (SoundInfos[PlayedSound].FileName <> '') then
  begin
    FAllocatedSource := SourceAllocator.AllocateSource(MaxSoundImportance);
    if FAllocatedSource <> nil then
    begin
      alCommonSourceSetup(FAllocatedSource.ALSource, true, false,
        SoundBuffers[PlayedSound],
        MusicVolume * SoundInfos[PlayedSound].Gain, 0, 1);

      alSourcePlay(FAllocatedSource.ALSource);

      FAllocatedSource.OnUsingEnd :=
        {$ifdef FPC_OBJFPC} @ {$endif} AllocatedSourceUsingEnd;
    end;
  end;
end;

procedure TMusicPlayer.SetPlayedSound(const Value: TSoundType);
begin
  if Value <> FPlayedSound then
  begin
    if FAllocatedSource <> nil then
    begin
      FAllocatedSource.DoUsingEnd;
      { AllocatedSourceUsingEnd should set FAllocatedSource to nil. }
      Assert(FAllocatedSource = nil);
    end;

    FPlayedSound := Value;

    AllocateSource;
  end;
end;

procedure TMusicPlayer.AllocatedSourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender = FAllocatedSource);
  FAllocatedSource.OnUsingEnd := nil;
  FAllocatedSource := nil;
end;

{ Other non-class things ----------------------------------------------------- }

const
  DefaultMusicVolume = 1.0;

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
    if MusicPlayer.FAllocatedSource <> nil then
      alSourcef(MusicPlayer.FAllocatedSource.ALSource,
        AL_GAIN, MusicVolume * SoundInfos[MusicPlayer.PlayedSound].Gain);
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
  MusicPlayer := TMusicPlayer.Create;

  FSoundVolume := ConfigFile.GetValue('sound/volume', DefaultSoundVolume);
  FMusicVolume   := ConfigFile.GetValue('sound/music/volume', DefaultMusicVolume);
  FALMinAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/min', DefaultALMinAllocatedSources);
  FALMaxAllocatedSources := ConfigFile.GetValue(
    'sound/allocated_sources/max', DefaultALMaxAllocatedSources);
  ALCDevice := ConfigFile.GetValue('sound/device', BestALCDevice);
finalization
  ConfigFile.SetDeleteValue('sound/volume', SoundVolume, DefaultSoundVolume);
  ConfigFile.SetDeleteValue('sound/music/volume', MusicVolume, DefaultMusicVolume);
  ConfigFile.SetDeleteValue('sound/allocated_sources/min',
    FALMinAllocatedSources, DefaultALMinAllocatedSources);
  ConfigFile.SetDeleteValue('sound/allocated_sources/max',
    FALMaxAllocatedSources, DefaultALMaxAllocatedSources);
  ConfigFile.SetDeleteValue('sound/device', ALCDevice, BestALCDevice);

  FreeAndNil(MusicPlayer);
end.