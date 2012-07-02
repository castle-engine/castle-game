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

{ Items specific to this game. }
unit GameItems;

interface

uses Boxes3D, X3DNodes, VectorMath, CastleUtils,
  CastleClassUtils, Classes, Images, GL, GLU, CastleGLUtils, GameSound,
  PrecalculatedAnimation, CastleResources,
  CastleXMLConfig, ALSoundEngine, Base3D, CastleItems;

type
  TItemPotionOfLifeKind = class(TItemKind)
    procedure Use(Item: TItem); override;
  end;

  TItemSwordKind = class(TItemShortRangeWeaponKind)
  public
    procedure ActualAttack(Item: TItem; World: T3DWorld); override;
  end;

  TItemBowKind = class(TItemWeaponKind)
  public
    procedure ActualAttack(Item: TItem; World: T3DWorld); override;
  end;

  TItemScrollOfFlyingKind = class(TItemKind)
    procedure Use(Item: TItem); override;
  end;

var
  Sword: TItemKind;
  Bow: TItemKind;
  LifePotion: TItemKind;
  KeyItemKind: TItemKind;
  RedKeyItemKind: TItemKind;
  Quiver: TItemKind;

procedure ItemsKindsInit;

implementation

uses SysUtils, CastleWindow,
  GamePlay, CastleFilesUtils, ProgressUnit,
  GameCreatures, CastleGameNotifications, CastleConfig,
  GLImages, CastleCreatures;

{ TItemPotionOfLifeKind ---------------------------------------------------- }

procedure TItemPotionOfLifeKind.Use(Item: TItem);
begin
  if Player.Life < Player.MaxLife then
  begin
    Player.Life := Min(Player.Life + 50, Player.MaxLife);
    Notifications.Show(Format('You drink "%s"', [Item.Kind.Caption]));
    Item.Quantity := Item.Quantity - 1;
    SoundEngine.Sound(stPlayerPotionDrink);
  end else
    Notifications.Show('You feel quite alright, no need to waste this potion');
end;

{ TItemSwordKind ------------------------------------------------------------- }

procedure TItemSwordKind.ActualAttack(Item: TItem; World: T3DWorld);
var
  WeaponBoundingBox: TBox3D;
  I: Integer;
  C: TCreature;
begin
  { Player.Direction may be multiplied by something here for long-range weapons }
  WeaponBoundingBox := Player.BoundingBox.Translate(Player.Direction);
  { Tests: Writeln('WeaponBoundingBox is ', WeaponBoundingBox.ToNiceStr); }
  { TODO: we would prefer to use World.BoxCollision for this,
    but we need to know which creature was hit. }
  for I := 0 to World.Count - 1 do
    if World[I] is TCreature then
    begin
      C := TCreature(World[I]);
      { Tests: Writeln('Creature bbox is ', C.BoundingBox.ToNiceStr); }
      if C.BoundingBox.Collision(WeaponBoundingBox) then
      begin
        C.Hurt(DamageConst + Random * DamageRandom, Player.Direction, AttackKnockbackDistance);
      end;
    end;
end;

{ TItemBowKind ------------------------------------------------------------- }

procedure TItemBowKind.ActualAttack(Item: TItem; World: T3DWorld);
var
  QuiverIndex: Integer;
begin
  QuiverIndex := Player.Items.FindKind(Quiver);
  if QuiverIndex = -1 then
  begin
    Notifications.Show('You have no arrows');
    SoundEngine.Sound(stPlayerInteractFailed);
  end else
  begin
    { delete arrow from player }
    Player.Items[QuiverIndex].Quantity :=
      Player.Items[QuiverIndex].Quantity - 1;
    if Player.Items[QuiverIndex].Quantity = 0 then
      Player.DeleteItem(QuiverIndex).Free;

    { shoot the arrow }
    Arrow.CreateCreature(Player.World, Player.Position, Player.Direction);
    SoundEngine.Sound(stArrowFired);
  end;
end;

{ TItemScrollOfFlyingKind ---------------------------------------------------- }

procedure TItemScrollOfFlyingKind.Use(Item: TItem);
begin
  Notifications.Show(Format('You cast spell from "%s"', [Item.Kind.Caption]));
  Player.FlyingModeTimeoutBegin(30.0);
  Item.Quantity := Item.Quantity - 1;
  SoundEngine.Sound(stPlayerCastFlyingSpell);
end;

{ initialization / finalization ---------------------------------------- }

procedure ItemsKindsInit;
begin
  Sword := AllResources.FindId('Sword') as TItemKind;
  Bow := AllResources.FindId('Bow') as TItemKind;
  LifePotion := AllResources.FindId('LifePotion') as TItemKind;
  KeyItemKind := AllResources.FindId('Key') as TItemKind;
  RedKeyItemKind := AllResources.FindId('RedKey') as TItemKind;
  Quiver := AllResources.FindId('Quiver') as TItemKind;
end;

initialization
  RegisterResourceClass(TItemKind, 'Item');
  RegisterResourceClass(TItemSwordKind, 'Sword');
  RegisterResourceClass(TItemBowKind, 'Bow');
  RegisterResourceClass(TItemPotionOfLifeKind, 'LifePotion');
  RegisterResourceClass(TItemScrollOfFlyingKind, 'ScrFlying');
end.
