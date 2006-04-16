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

implementation

uses CastleConfig;

procedure Sound(SoundType: TSoundType);
begin
  if SoundType <> stNone then
  begin
    { TODO }
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

initialization
  FEffectsVolume := ConfigFile.GetValue('sound/effects/volume', DefaultEffectsVolume);
  FMusicVolume   := ConfigFile.GetValue('sound/music/volume', DefaultMusicVolume);
finalization
  ConfigFile.SetDeleteValue('sound/effects/volume', EffectsVolume, DefaultEffectsVolume);
  ConfigFile.SetDeleteValue('sound/music/volume', MusicVolume, DefaultMusicVolume);
end.