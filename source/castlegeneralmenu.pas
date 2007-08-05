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

{ }
unit CastleGeneralMenu;

interface

uses GLMenu;

type
  { Just TGLMenu that calls Glw.PostRedisplay and plays a sound
    on each CurrentItem change. }
  TCastleMenu = class(TGLMenu)
  public
    constructor Create;
    procedure CurrentItemChanged; override;
    procedure SomethingChanged; override;
    procedure CurrentItemSelected; override;
  end;

var
  DebugMenuDesigner: boolean = false;

implementation

uses SysUtils, GLWindow, CastleWindow, CastleSound;

{ TCastleMenu ---------------------------------------------------------------- }

constructor TCastleMenu.Create;
begin
  inherited;
  { Don't set DesignerModeWindow, we do tricks that make setting mouse
    position in GLMenu not working. See TGLMenu.DesignerMode comments. }
  DesignerMode := DebugMenuDesigner;
end;

procedure TCastleMenu.CurrentItemChanged;
begin
  inherited;
  SoundEngine.Sound(stMenuCurrentItemChanged);
end;

procedure TCastleMenu.CurrentItemSelected;
begin
  inherited;
  SoundEngine.Sound(stMenuCurrentItemSelected);
end;

procedure TCastleMenu.SomethingChanged;
begin
  inherited;
  Glw.PostRedisplay;
end;

{ initialization / finalization ---------------------------------------------- }

procedure CloseGLW(Glwin: TGLWindow);
begin
  GLMenuCloseGL;
end;

initialization
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.