{
  Copyright 2006-2013 Michalis Kamburelis.

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

uses CastleUtils, GameSound, CastleResources, CastleSoundEngine, CastleItems;

type
  TItemPotionOfLifeResource = class(TItemResource)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemPotionOfLife = class(TInventoryItem)
  protected
    procedure Use; override;
  end;

  TItemScrollOfFlyingResource = class(TItemResource)
  protected
    function ItemClass: TInventoryItemClass; override;
  end;

  TItemScrollOfFlying = class(TInventoryItem)
  protected
    procedure Use; override;
  end;

var
  Sword: TItemResource;
  Bow: TItemResource;
  LifePotion: TItemResource;
  KeyItemResource: TItemResource;
  RedKeyItemResource: TItemResource;
  Quiver: TItemResource;

procedure ItemsResourcesInit;

implementation

uses SysUtils, GamePlay, CastleGameNotifications;

{ TItemPotionOfLifeResource ------------------------------------------------------ }

function TItemPotionOfLifeResource.ItemClass: TInventoryItemClass;
begin
  Result := TItemPotionOfLife;
end;

{ TItemPotionOfLife ---------------------------------------------------------- }

procedure TItemPotionOfLife.Use;
begin
  if Player.Life < Player.MaxLife then
  begin
    Player.Life := Min(Player.Life + 50, Player.MaxLife);
    Notifications.Show(Format('You drink "%s"', [Resource.Caption]));
    Quantity := Quantity - 1;
    SoundEngine.Sound(stPlayerPotionDrink);
  end else
    Notifications.Show('You feel quite alright, no need to waste this potion');
end;

{ TItemScrollOfFlyingResource ---------------------------------------------------- }

function TItemScrollOfFlyingResource.ItemClass: TInventoryItemClass;
begin
  Result := TItemScrollOfFlying;
end;

{ TItemScrollOfFlying -------------------------------------------------------- }

procedure TItemScrollOfFlying.Use;
begin
  Notifications.Show(Format('You cast spell from "%s"', [Resource.Caption]));
  Player.FlyingTimeOut := 30.0;
  Quantity := Quantity - 1;
  SoundEngine.Sound(stPlayerCastFlyingSpell);
end;

{ initialization / finalization ---------------------------------------- }

procedure ItemsResourcesInit;
begin
  Sword := Resources.FindName('Sword') as TItemResource;
  Bow := Resources.FindName('Bow') as TItemResource;
  LifePotion := Resources.FindName('LifePotion') as TItemResource;
  KeyItemResource := Resources.FindName('Key') as TItemResource;
  RedKeyItemResource := Resources.FindName('RedKey') as TItemResource;
  Quiver := Resources.FindName('Quiver') as TItemResource;
end;

initialization
  RegisterResourceClass(TItemPotionOfLifeResource, 'LifePotion');
  RegisterResourceClass(TItemScrollOfFlyingResource, 'ScrFlying');
end.
