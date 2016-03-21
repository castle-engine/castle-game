{
  Copyright 2006-2014 Michalis Kamburelis.

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

{ }
unit GameInitialBackground;

interface

uses CastleGLImages, CastleUIState;

type
  TStateInitial = class(TUIState)
  strict private
    GLScreenImage: TGLImage;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure Render; override;
  end;

var
  StateInitial: TStateInitial;

implementation

uses SysUtils, CastleUIControls,
  CastleFilesUtils;

procedure TStateInitial.Start;
begin
  inherited;
  GLScreenImage := TGLImageManaged.Create(
    ApplicationData('menu_bg/initial_background.png'), []);
end;

procedure TStateInitial.Stop;
begin
  FreeAndNil(GLScreenImage);
  inherited;
end;

procedure TStateInitial.Render;
begin
  inherited;
  GLScreenImage.Draw(ScreenRect);
end;

end.
