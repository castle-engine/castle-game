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

{ Playing the game. }

unit CastlePlay;

interface

uses CastleLevel;

procedure ShowHelpMessage;

procedure PlayLevel(ALevel: TCastleLevel);

const
  Version = '0.2.0';

implementation

uses SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath, Boxes3d;

var
  GameCancelled: boolean;
  Level: TCastleLevel;

const
  ViewAngleDegX = 45.0;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX, Glw.Height / Glw.Width);
end;

{ If ALActive then update listener POSITION and ORIENTATION
  and GAIN based on Glw.NavWalker.Camera* }
procedure alUpdateListener;
begin
  if ALActive then
  begin
    alListenerVector3f(AL_POSITION, Glw.NavWalker.CameraPos);
    alListenerOrientation(Glw.NavWalker.CameraDir, Glw.NavWalker.CameraUp);
  end;
end;

procedure Resize(Glwin: TGLWindow);

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    Glw.NavWalker.ProjectionMatrix := ProjectionMatrix;
  end;

begin
  { update glViewport and projection }
  glViewport(0, 0, Glwin.Width, Glwin.Height);
  ProjectionGLPerspective(ViewAngleDegY, Glwin.Width / Glwin.Height,
    Level.ProjectionNear, Level.ProjectionFar);

  UpdateNavigatorProjectionMatrix;
end;

procedure Draw(Glwin: TGLWindow);
begin
  if Level.Scene.Background <> nil then
  begin
    glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
    Level.Scene.Background.Render;
    glClear(GL_DEPTH_BUFFER_BIT);
  end else
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadMatrix(Glw.Navigator.Matrix);

  Level.LightSet.RenderLights;

  Level.Scene.RenderFrustumOctree(Glw.NavWalker.Frustum);
end;

procedure Idle(Glwin: TGLWindow);
begin
end;

procedure Timer(Glwin: TGLWindow);
begin
  if ALActive then CheckAL('game loop (check in OnTimer)');
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);
begin
  case Key of
    K_F1: ShowHelpMessage;
    K_F5: Glwin.SaveScreen(FnameAutoInc(ApplicationName + '_screen_%d.png'));
    else
      case C of
        CharEscape: GameCancelled := true;
      end;
  end;
end;

function MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single): boolean;
begin
  Result :=
    Box3dPointInside(ProposedNewPos, Level.Scene.BoundingBox) and
    Level.Scene.DefaultTriangleOctree.MoveAllowed(
      Navigator.CameraPos, ProposedNewPos, NewPos, Level.CameraRadius);
end;

type
  TDummy = class
    class procedure MatrixChanged(Navigator: TMatrixNavigator);
  end;

class procedure TDummy.MatrixChanged(Navigator: TMatrixNavigator);
begin
  Glw.PostRedisplay;
  alUpdateListener;
end;


procedure ShowHelpMessage;
const
  HelpMessage = {$I help_message.inc};
begin
  MessageOK(Glw, HelpMessage + nl +
    ApplicationName + ' version ' + Version + '.' +nl+
    'Author: Michalis Kamburelis, aka Kambi <michalis@camelot.homedns.org>' +nl+
    { TODO: later I will just use here SCamelotProgramHelpSuffix,
      for now this program is not avail on camelot. }
    {'See http://www.camelot.homedns.org/~michalis/ for latest versions' +
    Iff(WrapLines, nl + ' ', '') +
    ' of this program, sources, documentation etc.' +nl+}
    'Compiled with ' + SCompilerDescription +'.',
    taLeft);
end;

procedure PlayLevel(ALevel: TCastleLevel);
var
  SavedMode: TGLMode;
  CamPos, CamDir, CamUp: TVector3Single;
begin
  Level := ALevel;

  SavedMode := TGLMode.Create(glw,
    { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
    GL_ENABLE_BIT);
  try
    { init navigator }
    Glw.Navigator := TMatrixWalker.Create(TDummy.MatrixChanged);
    try
      { Init Glw.NavWalker properties }
      Glw.NavWalker.Key_MoveSpeedInc := K_None; { turn key off }
      Glw.NavWalker.Key_MoveSpeedDec := K_None; { turn key off }
      Glw.NavWalker.OnMoveAllowed := MoveAllowed;

      { Init initial camera pos }
      Level.Scene.GetPerspectiveCamera(CamPos, CamDir, CamUp);
      VectorAdjustToLengthTo1st(CamDir, Level.CameraRadius * 0.5);
      Glw.NavWalker.Init(CamPos, CamDir, CamUp);

      SetStandardGLWindowState(Glw, Draw, nil{TODO CloseQuery}, Resize,
        nil, true, true, false, K_None, #0, true, true);

      Glw.OnIdle := Idle;
      Glw.OnTimer := Timer;
      Glw.OnKeyDown := KeyDown;

      Glw.EventResize;

      GameCancelled := false;

      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);

      MessageRectStipple := @ThreeQuartersStipple;
      try

        repeat
          Glwm.ProcessMessage(true);
        until GameCancelled;

      finally MessageRectStipple := nil; end;
    finally FreeAndNil(Glw.Navigator); end;
  finally FreeAndNil(SavedMode); end;
end;

end.