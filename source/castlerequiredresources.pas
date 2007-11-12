{
  Copyright 2007 Michalis Kamburelis.

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
unit CastleRequiredResources;

interface

uses Classes;

var
  { Should we conserve memory by keeping only the required creatures ?

    If not, all creatures will be loaded each time you start a game.
    If yes, RequireCreatures and UnRequireCreatures will load/free
    resources for creatures, to only keep creatures that are required
    at least once. }
  ConserveResourcesOnlyForCurrentLevel: boolean = true;

procedure RequireCreatures(Names: TStringList);
procedure UnRequireCreatures(Names: TStringList);

implementation

uses SysUtils, CastleCreatures, KambiLog, ProgressUnit;

{
procedure DebugOutputRequiredCounts;
var
  I: Integer;
begin
  for I := 0 to CreaturesKinds.Count - 1 do
    Writeln(CreaturesKinds.Items[I].VRMLNodeName, ' ',
      CreaturesKinds.Items[I].RequiredCount);
end;
}

procedure RequireCreatures(Names: TStringList);
var
  I: Integer;
  Kind: TCreatureKind;
  PrepareRenderSteps: Cardinal;
begin
  if ConserveResourcesOnlyForCurrentLevel then
  begin
    { We iterate two times over Names, first time only to calculate
      PrepareRenderSteps, 2nd time does actual work.
      1st time increments RequiredCount (as 2nd pass may be optimized
      out, if not needed). }

    PrepareRenderSteps := 0;
    for I := 0 to Names.Count - 1 do
    begin
      Kind := CreaturesKinds.FindByVRMLNodeName(Names[I]);
      Kind.RequiredCount := Kind.RequiredCount + 1;
      if Kind.RequiredCount = 1 then
      begin
        Assert(not Kind.PrepareRenderDone);
        PrepareRenderSteps += Kind.PrepareRenderSteps;
      end;
    end;

    if PrepareRenderSteps <> 0 then
    begin
      Progress.Init(PrepareRenderSteps, 'Loading creatures');
      try
        for I := 0 to Names.Count - 1 do
        begin
          Kind := CreaturesKinds.FindByVRMLNodeName(Names[I]);
          if Kind.RequiredCount = 1 then
          begin
            if Log then
              WritelnLog('resources required',
                Format('Creature "%s" becomes required, loading',
                [Names[I]]));

            Kind.PrepareRender;
          end;
        end;
      finally Progress.Fini end;
    end;
  end;
end;

procedure UnRequireCreatures(Names: TStringList);
var
  I: Integer;
  Kind: TCreatureKind;
begin
  if ConserveResourcesOnlyForCurrentLevel then
  begin
    for I := 0 to Names.Count - 1 do
    begin
      Kind := CreaturesKinds.FindByVRMLNodeName(Names[I]);

      { If everything went OK, I could place here an assertion
        Assert(Kind.RequiredCount > 0);
        However, if creature loading inside RequireCreatures will fail,
        then TLevel destructor is forced to call UnRequireCreatures
        possibly "unrequiring" more than was actually initialized.
        So we have to silently ignore cases when creature is unrequired
        even though it already has "Kind.RequiredCount = 0".
        Still, a correct run of the program (when creature loading goes 100% OK)
        should always have Kind.RequiredCount > 0 here. }

      if Kind.RequiredCount = 0 then Exit;

      Kind.RequiredCount := Kind.RequiredCount - 1;
      if Kind.RequiredCount = 0 then
      begin
        if Log then
          WritelnLog('resources required',
            Format('Creature "%s" is no longer required, freeing',
            [Names[I]]));
        Assert(Kind.PrepareRenderDone);

        Kind.FreePrepareRender;
      end;
    end;
  end;
end;

end.
