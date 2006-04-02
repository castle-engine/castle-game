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

    { Equipping items sounds.
      @groupBegin }
    stEquippingSword,
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
    stBallMissileFired,
    stBallMissileExplode,
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

implementation

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

end.