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

{ This keeps global Glw variable.

  This is just like GLW_Navigated, but with some specific customizations
  for this project. }

unit CastleWindow;

interface

uses GLWindow, VRMLOpenGLRenderer;

var
  { @noAutoLinkHere }
  Glw: TGLWindowNavigated;

const
  RequiredScreenWidth = 800;
  RequiredScreenHeight = 600;

  MiddleScreenWidth = RequiredScreenWidth div 2;
  MiddleScreenHeight = RequiredScreenHeight div 2;

function RequiredScreenSize: string;

var
  GLContextCache: TVRMLOpenGLRendererContextCache;

implementation

uses SysUtils;

function RequiredScreenSize: string;
begin
  Result := Format('%d x %d', [RequiredScreenWidth, RequiredScreenHeight]);
end;

{ initialization / finalization ---------------------------------------------- }

initialization
  Glw := TGLWindowNavigated.Create;
  Glw.OwnsNavigator := false;

  GLContextCache := TVRMLOpenGLRendererContextCache.Create;
  GLContextCache.UseTextureFileNames := true;
finalization
  FreeAndNil(GLContextCache);

  FreeAndNil(Glw);
end.
