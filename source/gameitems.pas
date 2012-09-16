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
  CastleXMLConfig, CastleSoundEngine, Base3D, CastleItems;

type
  TItemPotionOfLifeKind = class(TItemKind)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemPotionOfLife = class(TInventoryItem)
  public
    procedure Use; override;
  end;

  TItemSwordKind = class(TItemShortRangeWeaponKind)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemSword = class(TItemWeapon)
  public
    procedure ActualAttack; override;
  end;

  TItemBowKind = class(TItemWeaponKind)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemBow = class(TItemWeapon)
  public
    procedure ActualAttack; override;
  end;

  TItemScrollOfFlyingKind = class(TItemKind)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemScrollOfFlying = class(TInventoryItem)
  public
    procedure Use; override;
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

{ TItemPotionOfLifeKind ------------------------------------------------------ }

function TItemPotionOfLifeKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemPotionOfLife;
end;

{ TItemPotionOfLife ---------------------------------------------------------- }

procedure TItemPotionOfLife.Use;
begin
  if Player.Life < Player.MaxLife then
  begin
    Player.Life := Min(Player.Life + 50, Player.MaxLife);
    Notifications.Show(Format('You drink "%s"', [Kind.Caption]));
    Quantity := Quantity - 1;
    SoundEngine.Sound(stPlayerPotionDrink);
  end else
    Notifications.Show('You feel quite alright, no need to waste this potion');
end;

{ TItemSwordKind ------------------------------------------------------------- }

function TItemSwordKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemSword;
end;

{ TItemSword ----------------------------------------------------------------- }

procedure TItemSword.ActualAttack;
var
  WeaponBoundingBox: TBox3D;
  I: Integer;
  C: TCreature;
  K: TItemShortRangeWeaponKind;
  O: T3DOrient;
begin
  if (Owner3D <> nil) and
     (Owner3D is T3DOrient) then
  begin
    O := T3DOrient(Owner3D); 
    { O.Direction may be multiplied by something here for long-range weapons }
    WeaponBoundingBox := O.BoundingBox.Translate(O.Direction);
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
          K := Kind as TItemShortRangeWeaponKind;
          C.Hurt(K.DamageConst + Random * K.DamageRandom, O.Direction,
            K.AttackKnockbackDistance);
        end;
      end;
  end;
end;

{ TItemBowKind ------------------------------------------------------------- }

function TItemBowKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemBow;
end;

{ TItemBow ------------------------------------------------------------------- }

procedure TItemBow.ActualAttack;
var
  QuiverIndex: Integer;
  QuiverItem: TInventoryItem;
begin
  QuiverIndex := Player.Inventory.FindKind(Quiver);
  if QuiverIndex = -1 then
  begin
    Notifications.Show('You have no arrows');
    SoundEngine.Sound(stPlayerInteractFailed);
  end else
  begin
    { delete arrow from player }
    QuiverItem := Player.Inventory[QuiverIndex];
    QuiverItem.Quantity := QuiverItem.Quantity - 1;
    Player.Inventory.CheckDepleted(QuiverItem);

    { shoot the arrow }
    Arrow.CreateCreature(World, Player.Position, Player.Direction);
    SoundEngine.Sound(stArrowFired);
  end;
end;

{ TItemScrollOfFlyingKind ---------------------------------------------------- }

function TItemScrollOfFlyingKind.ItemClass: TInventoryItemClass;
begin
  Result := TItemScrollOfFlying;
end;

{ TItemScrollOfFlying -------------------------------------------------------- }

procedure TItemScrollOfFlying.Use;
begin
  Notifications.Show(Format('You cast spell from "%s"', [Kind.Caption]));
  Player.FlyingModeTimeoutBegin(30.0);
  Quantity := Quantity - 1;
  SoundEngine.Sound(stPlayerCastFlyingSpell);
end;

{ initialization / finalization ---------------------------------------- }

procedure ItemsKindsInit;
begin
  Sword := Resources.FindName('Sword') as TItemKind;
  Bow := Resources.FindName('Bow') as TItemKind;
  LifePotion := Resources.FindName('LifePotion') as TItemKind;
  KeyItemKind := Resources.FindName('Key') as TItemKind;
  RedKeyItemKind := Resources.FindName('RedKey') as TItemKind;
  Quiver := Resources.FindName('Quiver') as TItemKind;
end;

initialization
  RegisterResourceClass(TItemKind, 'Item');
  RegisterResourceClass(TItemSwordKind, 'Sword');
  RegisterResourceClass(TItemBowKind, 'Bow');
  RegisterResourceClass(TItemPotionOfLifeKind, 'LifePotion');
  RegisterResourceClass(TItemScrollOfFlyingKind, 'ScrFlying');
end.
