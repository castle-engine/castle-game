unit CastlePlay;

interface

uses CastleLevel;

procedure PlayLevel(ALevel: TCastleLevel);

const
  Version = '0.2.0';
  DisplayProgramName = 'castle';

implementation

uses SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath;

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

  { TODO -- decide: either move some enables/disables below to Init/Close ?
    Or leave them here for some reason ? Just like with lets_take_a_walk. }
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  Level.Scene.RenderFrustumOctree(Glw.NavWalker.Frustum);

  glDisable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
end;

procedure Idle(Glwin: TGLWindow);
begin
end;

procedure Timer(Glwin: TGLWindow);
begin
  if ALActive then CheckAL('game loop (check in OnTimer)');
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);

  procedure ShowHelpMessage;
  const
    HelpMessage = {$I help_message.inc};
  begin
    MessageOK(Glw, HelpMessage + nl +
      SCamelotProgramHelpSuffix(DisplayProgramName, Version, false), taLeft);
  end;

begin
  case Key of
    K_F1: ShowHelpMessage;
    K_F5: Glwin.SaveScreen(FnameAutoInc(DisplayProgramName + '_screen_%d.png'));
    else
      case C of
        CharEscape: GameCancelled := true;
      end;
  end;
end;

function MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single): boolean;
begin
  Result := Level.Scene.DefaultTriangleOctree.MoveAllowed(
    Navigator.CameraPos, ProposedNewPos, NewPos, Level.CameraRadius);
  { TODO -- put some constraints here, like in lets_take_a_walk,
    to not allow him to get outside of level box. }
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

procedure PlayLevel(ALevel: TCastleLevel);
var
  SavedMode: TGLMode;
  CamPos, CamDir, CamUp: TVector3Single;
begin
  Level := ALevel;

  SavedMode := TGLMode.Create(glw, 0);
  try
    { init navigator }
    Glw.Navigator := TMatrixWalker.Create(TDummy.MatrixChanged);
    try
      Level.Scene.GetPerspectiveCamera(CamPos, CamDir, CamUp);
      VectorAdjustToLengthTo1st(CamDir, Level.CameraRadius * 2);
      Glw.NavWalker.Init(CamPos, CamDir, CamUp);
      Glw.NavWalker.OnMoveAllowed := MoveAllowed;

      SetStandardGLWindowState(Glw, Draw, nil{TODO CloseQuery}, Resize,
        nil, true, true, false, #0, #0, true, true);

      Glw.OnIdle := Idle;
      Glw.OnTimer := Timer;
      Glw.OnKeyDown := KeyDown;
      
      Glw.EventResize;

      GameCancelled := false;

      repeat
        Glwm.ProcessMessage(true);
      until GameCancelled;

    finally FreeAndNil(Glw.Navigator); end;
  finally FreeAndNil(SavedMode); end;
end;

end.