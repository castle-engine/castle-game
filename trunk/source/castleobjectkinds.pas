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
unit CastleObjectKinds;

interface

type
  { This is a common class for item kind and creature kind. }
  TObjectKind = class
  private
    FTransparent: boolean;
  public
    constructor Create;

    { Prepare anything needed when starting new game.
      It can call Progress.Step PrepareRenderSteps times. }
    procedure PrepareRender; virtual;

    function PrepareRenderSteps: Cardinal; virtual;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;

    { Should the creature be rendered as transparent or opaque ?
      Each item/creature should either use only partially-transparent
      materials or only fully opaque materials. For reasoning, see
      CastlePlay.Draw routine. }
    property Transparent: boolean
      read FTransparent write FTransparent default false;
  end;

implementation

constructor TObjectKind.Create;
begin
  inherited;
  FTransparent := false;
end;

procedure TObjectKind.PrepareRender;
begin
  { Nothing to do in this class. }
end;

function TObjectKind.PrepareRenderSteps: Cardinal;
begin
  Result := 0;
end;

procedure TObjectKind.CloseGL;
begin
  { Nothing to do in this class. }
end;

end.