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

{ Handling of the background level displayed under the start menu.

  The idea is to reuse here most of our engine, so e.g. we have here
  a "real" game level --- TLevel instance and other things.
  Eventually, we will also add here creatures and items from our game,
  so we can make this background level to really show off some features and tease
  the player before (s)he clicks "New Game".
  At the same time, I have here the ability to insert some special
  things that cannot be really added to the game (e.g. some 2D effect
  that depends that the camera is on particular position --- in actual
  game we can't guarantee this, but in the game we can just set camera
  still).

  So this unit is somewhat equivalent to CastlePlay unit,
  but different. CastlePlay unit has global Player and Level instances.
  This unit doesn't use them (so it's a design decision that this
  unit @italic(doesn't use CastlePlay unit (even in the implementation))).
  This unit has internal BackgroundLevel instance and such.
}
unit CastleBackgroundLevel;

interface

uses GLWindow;

procedure BackgroundLevelBegin;
procedure BackgroundLevelEnd;

{ Draw background level.

  @italic(This procedure doesn't require you to set any particular
  3d projection matrix). This procedure will always set it's own
  3d projection matrix at the beginning (using matrix push/pop mechanism
  to keep your original projection unchanged at the end).
  This makes it possible to just call this proecure when your current
  projection is set to some 2d projection --- as is the case when
  we're at game's start menu.

  Actually, this will draw some 2d item: game caption.
}
procedure BackgroundLevelDraw(Glwin: TGLWindow);

procedure BackgroundLevelIdle(Glwin: TGLWindow);

implementation

uses SysUtils,
  Navigation, GL, GLU, GLExt, BackgroundGL, KambiGLUtils, GLImages,
  VRMLGLHeadlight, KambiFilesUtils, Images, VectorMath,
  CastleWindow, CastleLevel, CastleLevelAvailable, CastleVideoOptions,
  VRMLGLScene, RenderStateUnit;

var
  BackgroundLevel: TLevel;
  BackgroundNavigator: TWalkNavigator;

  GLList_Caption: TGLuint;
  {CaptionWidth, }CaptionHeight: Cardinal;

procedure BackgroundLevelBegin;
begin
  { initialize BackgroundLevel }
  BackgroundLevel := LevelsAvailable.FindName(LevelsAvailable.MenuBackgroundLevelName).
    CreateLevel(true);

  { initialize BackgroundNavigator }
  BackgroundNavigator := TWalkNavigator.Create(nil);
  BackgroundNavigator.Init(BackgroundLevel.InitialCameraPos,
    BackgroundLevel.InitialCameraDir,
    BackgroundLevel.InitialCameraUp,
    BackgroundLevel.GravityUp, 0.0, 0.0 { unused, we don't use Gravity here });
end;

procedure BackgroundLevelEnd;
begin
  FreeAndNil(BackgroundLevel);
  FreeAndNil(BackgroundNavigator);
end;

function ProjectionFar: Single;
begin
  if RenderShadowsPossible and RenderShadows then
    Result := ZFarInfinity else
    Result := BackgroundLevel.ProjectionFar;
end;

procedure BackgroundLevelDraw(Glwin: TGLWindow);

  procedure ProjectionPushSet;

    procedure UpdateNavigatorProjectionMatrix;
    var
      ProjectionMatrix: TMatrix4f;
    begin
      glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
      BackgroundNavigator.ProjectionMatrix := ProjectionMatrix;
    end;

  begin
    glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glMultMatrix(PerspectiveProjMatrixDeg(
        ViewAngleDegY, Glwin.Width / Glwin.Height,
        BackgroundLevel.ProjectionNear, ProjectionFar));
    glMatrixMode(GL_MODELVIEW);
    UpdateNavigatorProjectionMatrix;
  end;

  procedure ProjectionPop;
  begin
    glMatrixMode(GL_PROJECTION);
      glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;

var
  ClearBuffers: TGLbitfield;
  UsedBackground: TBackgroundGL;
begin
  glPushAttrib(GL_ENABLE_BIT);

    ProjectionPushSet;
    try

      ClearBuffers := GL_DEPTH_BUFFER_BIT;
      UsedBackground := BackgroundLevel.Background;

      if UsedBackground <> nil then
      begin
        glLoadMatrix(BackgroundNavigator.RotationMatrix);
        UsedBackground.Render;
      end else
        ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

      glClear(ClearBuffers);

      RenderState.CameraFromNavigator(BackgroundNavigator);
      glLoadMatrix(BackgroundNavigator.Matrix);

      { Set headlight }
      TVRMLGLHeadlight.RenderOrDisable(BackgroundLevel.Scene.Headlight, 0,
        true, BackgroundNavigator);

      BackgroundLevel.LightSet.RenderLights;

      glPushAttrib(GL_ENABLE_BIT);
        glEnable(GL_LIGHTING);
        BackgroundLevel.Render(BackgroundNavigator.Frustum, tgAll);
      glPopAttrib;

    finally ProjectionPop end;

    glLoadIdentity;
    glRasterPos2i(0, Glwin.Height - CaptionHeight);

    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.5);
    glCallList(GLList_Caption);

  glPopAttrib;
end;

procedure BackgroundLevelIdle(Glwin: TGLWindow);
var
  CompSpeed: Single;
begin
  CompSpeed := Glwin.Fps.IdleSpeed;
  BackgroundLevel.Idle(CompSpeed);
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
var
  ImageCaption: TImage;
begin
  ImageCaption := LoadImage(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'caption.png', [], [], 0, 0);
  try
    GLList_Caption := ImageDrawToDisplayList(ImageCaption);
    {CaptionWidth := ImageCaption.Width; useless for now}
    CaptionHeight := ImageCaption.Height;
  finally FreeAndNil(ImageCaption) end;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  glFreeDisplayList(GLList_Caption);
end;

initialization
  Glw.OnInitList.Add(@InitGLW);
  Glw.OnCloseList.Add(@CloseGLW);
finalization
end.