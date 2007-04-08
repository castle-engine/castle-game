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

{ Handling of the demo displayed under the start menu.

  The idea is to reuse here most of our engine, so e.g. we have here
  a "real" game level --- TLevel instance and other things.
  Eventually, we will also add here creatures and items from our game,
  so we can make this demo to really show off some features and tease
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
  This unit has internal DemoLevel instance and such.
}
unit CastleDemoLevel;

interface

uses GLWindow;

procedure DemoLevelBegin;
procedure DemoLevelEnd;

{ Draw demo level.

  @italic(This procedure doesn't require you to set any particular
  3d projection matrix). This procedure will always set it's own
  3d projection matrix at the beginning (using matrix push/pop mechanism
  to keep your original projection unchanged at the end).
  This makes it possible to just call this proecure when your current
  projection is set to some 2d projection --- as is the case when
  we're at game's start menu.

  Actually, this will draw some 2d item: game caption.
}
procedure DemoLevelDraw(Glwin: TGLWindow);

implementation

uses SysUtils,
  MatrixNavigation, OpenGLh, BackgroundGL, KambiGLUtils, VRMLGLHeadlight,
  KambiFilesUtils, Images,
  CastleWindow, CastleLevel, CastleLevelAvailable, CastleVideoOptions;

var
  DemoLevel: TLevel;
  DemoNavigator: TMatrixWalker;

  GLList_Caption: TGLuint;
  CaptionWidth, CaptionHeight: Cardinal;

procedure DemoLevelBegin;
begin
  { initialize DemoLevel }
  DemoLevel := LevelsAvailable.FindName(LevelsAvailable.DemoLevelName).CreateLevel;

  { initialize DemoNavigator }
  DemoNavigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
  DemoNavigator.Init(DemoLevel.HomeCameraPos,
    DemoLevel.HomeCameraDir,
    DemoLevel.HomeCameraUp, 0.0, 0.0 { unused, we don't use Gravity here });
end;

procedure DemoLevelEnd;
begin
  FreeAndNil(DemoLevel);
  FreeAndNil(DemoNavigator);
end;

procedure DemoLevelDraw(Glwin: TGLWindow);

  procedure ProjectionPushSet;

    procedure UpdateNavigatorProjectionMatrix;
    var
      ProjectionMatrix: TMatrix4f;
    begin
      glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
      DemoNavigator.ProjectionMatrix := ProjectionMatrix;
    end;

  begin
    glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      gluPerspective(ViewAngleDegY, Glwin.Width / Glwin.Height,
        DemoLevel.ProjectionNear, DemoLevel.ProjectionFar);
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
      UsedBackground := DemoLevel.Background;

      if UsedBackground <> nil then
      begin
        glLoadMatrix(DemoNavigator.RotationOnlyMatrix);
        UsedBackground.Render;
      end else
        ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

      glClear(ClearBuffers);

      { Set headlight. Must be done before glLoadMatrix(DemoNavigator.Matrix)
        below, since TVRMLGLHeadlight.RenderOrDisable may change the matrix. }
      TVRMLGLHeadlight.RenderOrDisable(DemoLevel.Headlight, 0);

      glLoadMatrix(DemoNavigator.Matrix);

      DemoLevel.LightSet.RenderLights;

      glPushAttrib(GL_ENABLE_BIT);
        glEnable(GL_LIGHTING);
        DemoLevel.Render(DemoNavigator.Frustum);
      glPopAttrib;

    finally ProjectionPop end;

    glLoadIdentity;
    glRasterPos2i(0, Glwin.Height - CaptionHeight);

    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.5);
    glCallList(GLList_Caption);

  glPopAttrib;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
var
  ImageCaption: TImage;
begin
  ImageCaption := LoadImage(ProgramDataPath + 'data' +
    PathDelim + 'menu_bg' + PathDelim + 'caption.png', [], [], 0, 0);
  try
    GLList_Caption := ImageDrawToDispList(ImageCaption);
    CaptionWidth := ImageCaption.Width;
    CaptionHeight := ImageCaption.Height;
  finally FreeAndNil(ImageCaption) end;
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  glFreeDisplayList(GLList_Caption);
end;

initialization
  Glw.OnInitList.AppendItem(@InitGLW);
  Glw.OnCloseList.AppendItem(@CloseGLW);
finalization
end.