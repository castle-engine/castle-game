{
  Copyright 2007-2012 Michalis Kamburelis.

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

{ Background stuff displayed under the start menu.

  To allow a wide range of 2D and 3D effects, we simply initialize here
  a full castle level. This level may be animated,
  it can even have some interactive stuff (like touch sensors etc.,
  although not used now).
  So we can make this background level to really show off some features
  and tease the player before (s)he clicks "New Game".
  At the same time, I have here the ability to insert some special
  things that cannot be really added to the game (e.g. some 2D effect
  that depends that the camera is on particular position --- in actual
  game we can't guarantee this, but in the game we can just set camera
  still).

  We could also place some creatures / items on this level
  (although not done for now, as we defer loading creatures / items
  until actual game).

  So this unit is somewhat equivalent to GamePlay unit,
  but different. GamePlay unit has global Player and Level instances.
  This unit doesn't use them (so it's a design decision that this
  unit @italic(doesn't use GamePlay unit (even in the implementation))).
  This unit has own TGameSceneManager instance (and no player, articial
  camera is created by BackgroundCreate).
}
unit GameBackgroundLevel;

interface

uses CastleWindow, CastleUIControls, CastleLevels;

var
  BackgroundControls: TUIControlList;
  BackgroundSceneManager: TGameSceneManager;

const
  { hardcoded for now }
  MenuBackgroundLevelName = 'gate_background';

{ Create / destroy BackgroundControls instances.
  @groupBegin }
procedure BackgroundCreate;
procedure BackgroundDestroy;
{ @groupEnd }

implementation

uses SysUtils, GL, GLU, GLExt, CastleGLUtils, CastleGLImages,
  CastleFilesUtils, CastleImages, CastleVectors, CastlePlayer;

{ TBackgroundCaptions -------------------------------------------------------- }

type
  TBackgroundCaptions = class(TUIControl)
  private
    GLCaption: TGLImage;
  public
    function DrawStyle: TUIControlDrawStyle; override;
    procedure Draw; override;
    procedure GLContextOpen; override;
    procedure GLContextClose; override;
  end;

function TBackgroundCaptions.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TBackgroundCaptions.Draw;
begin
  glPushAttrib(GL_ENABLE_BIT);
    glRasterPos2i(0, ContainerHeight - GLCaption.Height);

    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.5);
    GLCaption.Draw;
  glPopAttrib;
end;

procedure TBackgroundCaptions.GLContextOpen;
begin
  inherited;
  if GLCaption = nil then
    GLCaption := TGLImage.Create(ProgramDataPath + 'data' +
      PathDelim + 'menu_bg' + PathDelim + 'caption.png', []);
end;

procedure TBackgroundCaptions.GLContextClose;
begin
  FreeAndNil(GLCaption);
  inherited;
end;

{ routines ------------------------------------------------------------------- }

procedure BackgroundCreate;
var
  BackgroundCaptions: TUIControl;
  BackgroundPlayer: TPlayer;
begin
  BackgroundControls := TUIControlList.Create(true);

  { initialize BackgroundSceneManager }
  BackgroundSceneManager := TGameSceneManager.Create(nil);

  BackgroundSceneManager.LoadLevel(MenuBackgroundLevelName);
  BackgroundControls.Add(BackgroundSceneManager);

  { Do not allow to move the camera in any way. }
  BackgroundSceneManager.Camera.Input := [];

  { Disable interaction with the scene pointing device sensors by having
    player with Blocked = true. }
  BackgroundPlayer := TPlayer.Create(BackgroundSceneManager);
  BackgroundPlayer.Blocked := true;
  BackgroundSceneManager.Player := BackgroundPlayer;
  BackgroundSceneManager.Items.Add(BackgroundPlayer);

  BackgroundCaptions := TBackgroundCaptions.Create(nil);
  BackgroundControls.Add(BackgroundCaptions);
end;

procedure BackgroundDestroy;
begin
  FreeAndNil(BackgroundControls);
end;

end.
