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

{ TLevelAvailable class and related things. }
unit CastleLevelAvailable;

interface

uses CastleLevel, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TLevelAvailable = class
  public
    AvailableForNewGame: boolean;
    DefaultAvailableForNewGame: boolean;

    { Class to use when constructing this level. }
    LevelClass: TLevelClass;

    { These will be passed to TLevel constructor --- see appropriate TLevel
      properties for description. }
    Name: string;
    SceneFileName: string;
    LightSetFileName: string;
    Title: string;
    Number: Integer;

    function CreateLevel: TLevel;
  end;

  TObjectsListItem_1 = TLevelAvailable;
  {$I objectslist_1.inc}
  TLevelsAvailableList = class(TObjectsList_1)
  private
    function IsSmallerByNumber(const A, B: TLevelAvailable): boolean;
  public
    { raises Exception if such Name is not on the list. }
    function FindName(const AName: string): TLevelAvailable;

    procedure SortByNumber;

    procedure LoadFromConfig;
    procedure SaveToConfig;
  end;

var
  { This lists all available TLevel classes, along with information
    whether they are allowed to be placed in "New Game" levels.

    Created in initialization of this unit, destroyed in finalization.
    Owns it's Items. }
  LevelsAvailable: TLevelsAvailableList;

{$undef read_interface}

implementation

uses SysUtils, CastleConfig;

{$define read_implementation}
{$I objectslist_1.inc}

{ TLevelAvailable ------------------------------------------------------------ }

function TLevelAvailable.CreateLevel: TLevel;
begin
  Result := LevelClass.Create(Name, SceneFileName, LightSetFileName,
    Title, Number);
  AvailableForNewGame := true;
end;

{ TLevelsAvailableList ------------------------------------------------------- }

function TLevelsAvailableList.FindName(const AName: string): TLevelAvailable;
var
  I: Integer;
begin
  for I := 0 to High do
    if Items[I].Name = AName then
      Exit(Items[I]);

  raise Exception.CreateFmt(
    'Level "%s" not found on LevelsAvailable list', [AName]);
end;

function TLevelsAvailableList.IsSmallerByNumber(
  const A, B: TLevelAvailable): boolean;
begin
  Result := A.Number < B.Number;
end;

procedure TLevelsAvailableList.SortByNumber;
begin
  Sort(@IsSmallerByNumber);
end;

procedure TLevelsAvailableList.LoadFromConfig;
var
  I: Integer;
begin
  for I := 0 to High do
    Items[I].AvailableForNewGame := ConfigFile.GetValue(
      'levels_available/' + Items[I].Name,
      Items[I].DefaultAvailableForNewGame);
end;

procedure TLevelsAvailableList.SaveToConfig;
var
  I: Integer;
begin
  for I := 0 to High do
    ConfigFile.SetDeleteValue(
      'levels_available/' + Items[I].Name,
      Items[I].AvailableForNewGame,
      Items[I].DefaultAvailableForNewGame);
end;

initialization
  LevelsAvailable := TLevelsAvailableList.Create;
  { LevelsAvailable.LoadFromConfig; will be called from main program,
    because it must be called after LevelsAvailable list if filled. }
finalization
  LevelsAvailable.SaveToConfig;
  FreeWithContentsAndNil(LevelsAvailable);
end.