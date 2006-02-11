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

program castle;

uses OpenGLh, GLWindow, VRMLFlatSceneGL, VRMLNodes, SysUtils,
  KambiUtils, GLW_Navigated, KambiGLUtils, Boxes3d, MatrixNavigation, VectorMath,
  ProgressUnit, ProgressConsole, OpenAL, ALUtils,
  VRMLTriangleOctree, VRMLRayTracer, BackgroundGL,
  VRMLLightSetGL, ParseParametersUnit, Images,
  GLWinMessages;

{ global vars ------------------------------------------------------------ }

var
  BaseScene: TVRMLFlatSceneGL;
  CameraRadius: Single;

{ some funcs ------------------------------------------------------------ }

const
  ViewAngleDegX = 45.0;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX, Glw.Height / Glw.Width);
end;

function ProjectionNear: Single;
begin
  Result := CameraRadius * 0.75;
end;

function ProjectionFar: Single;
begin
  Result := Box3dMaxSize(BaseScene.BoundingBox) * 5;
end;

procedure alUpdateListener;
{ If ALActive then update listener POSITION and ORIENTATION
  and GAIN based on Glw.NavWalker.Camera* }
begin
  if ALActive then
  begin
    alListenerVector3f(AL_POSITION, Glw.NavWalker.CameraPos);
    alListenerOrientation(Glw.NavWalker.CameraDir, Glw.NavWalker.CameraUp);
  end;
end;

{ help message ------------------------------------------------------------ }

const
  Version = '0.2.0';
  DisplayProgramName = 'castle';

procedure ShowHelpMessage;
const
  HelpMessage = {$I help_message.inc};
begin
  MessageOK(Glw, HelpMessage + nl +
    SCamelotProgramHelpSuffix(DisplayProgramName, Version, false), taLeft);
end;

{ Glw callbacks ----------------------------------------------------- }

procedure Init(Glwin: TGLWindow);
begin
  BaseScene.BackgroundSkySphereRadius := TBackgroundGL.NearFarToSkySphereRadius
    (ProjectionNear, ProjectionFar);
  BaseScene.PrepareRender(true);
end;

procedure Close(Glwin: TGLWindow);
begin
  BaseScene.CloseGL;
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
    ProjectionNear, ProjectionFar);

  UpdateNavigatorProjectionMatrix;
end;

procedure Draw(Glwin: TGLWindow);
begin
  if BaseScene.Background <> nil then
  begin
    glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
    BaseScene.Background.Render;
    glClear(GL_DEPTH_BUFFER_BIT);
  end else
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadMatrix(Glw.Navigator.Matrix);

  { TODO -- decide: either move some enables/disables below to Init/Close ?
    Or leave them here for some reason ? Just like with lets_take_a_walk. }
  { TODO -- Probably I will want to separate lights into separate VRML,
    like in lets_take_a_walk. }
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  BaseScene.RenderFrustumOctree(Glw.NavWalker.Frustum);

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
begin
  case Key of
    K_F1: ShowHelpMessage;
    K_F5: Glwin.SaveScreen(FnameAutoInc(DisplayProgramName + '_screen_%d.png'));
  end;
end;

{ other funcs ------------------------------------------------------------ }

function LoadVRMLNode(const Name: string): TVRMLNode;
begin
  Result := ParseVRMLFile(ProgramDataPath + 'data' + PathDelim + Name, false);
end;

function LoadModel(const Name: string;
  RendererOptimization: TGLRendererOptimization = roSceneAsAWhole):
  TVRMLFlatSceneGL;
begin
  Result := TVRMLFlatSceneGL.Create(LoadVRMLNode(Name), true,
    RendererOptimization);
  Result.Attrib_TextureMinFilter := GL_LINEAR_MIPMAP_LINEAR;
  Result.Attrib_UseLights := true;
  Result.Attrib_FirstGLFreeLight := 1;
end;

function MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single): boolean;
begin
  Result := BaseScene.DefaultTriangleOctree.MoveAllowed(
    Navigator.CameraPos, ProposedNewPos, NewPos, CameraRadius);
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

{ Do this after successfull TryBeginAL.
  Glw.NavWalker must be already created and initialized
  (we want this to quickly set up initial listener properties;
  otherwise user could for some short time at start hear wrong sounds,
  because we start here some looping sounds). }
procedure InitAL;
begin
 alUpdateListener;

 CheckAL('init OpenAL');
end;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoSound: boolean = false;

const
  Options: array[0..2]of TOption =
  ((Short:'h'; Long: 'help'; Argument: oaNone),
   (Short: #0; Long: 'no-sound'; Argument: oaNone),
   (Short:'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
 case OptionNum of
  0: begin
      InfoWrite(
        'castle.' +nl+
        nl+
        'Options:' +nl+
        HelpOptionHelp +nl+
        VersionOptionHelp +nl+
        OpenALOptionsHelp(true) +nl+
        '  --no-sound            Turn off sound' +nl+
        nl+
        TGLWindow.ParseParametersHelp(StandardParseOptions, true) +nl+
        nl+
        SCamelotProgramHelpSuffix(DisplayProgramName, Version, true));
      ProgramBreak;
     end;
  1: WasParam_NoSound := true;
  2: begin
      Writeln(Version);
      ProgramBreak;
     end;
 end;
end;

{ main -------------------------------------------------------------------- }

var
  CamPos, CamDir, CamUp: TVector3Single;
begin
 try
  { parse parameters }
  Glw.FullScreen := true; { by default we open in fullscreen }
  Glw.ParseParameters(StandardParseOptions);
  OpenALOptionsParse;
  ParseParameters(Options, OptionProc, nil);

  { load VRMLs }
  BaseScene := LoadModel('castle_hall_final.wrl', roSeparateShapeStates{roSceneAsAWhole});
  { TODO-fix this, use only GL progress.
    Under Win32, program should not create console. }
  Progress.UserInterface := ProgressConsoleInterface;
  BaseScene.DefaultTriangleOctree :=
    BaseScene.CreateTriangleOctree('Building triangle octree');
  BaseScene.DefaultShapeStateOctree :=
    BaseScene.CreateShapeStateOctree('Building ShapeState octree');

  { init navigator }
  Glw.Navigator := TMatrixWalker.Create(TDummy.MatrixChanged);
  BaseScene.GetPerspectiveCamera(CamPos, CamDir, CamUp);
  CameraRadius := Box3dAvgSize(BaseScene.BoundingBox) * 0.015;
  VectorAdjustToLengthTo1st(CamDir, CameraRadius * 2);
  Glw.NavWalker.Init(CamPos, CamDir, CamUp);
  Glw.NavWalker.OnMoveAllowed := MoveAllowed;

  { init OpenAL (must be after initing navigator - InitAL requires navigator
    to be already inited) }
  if WasParam_NoSound then
   Writeln('Sound disabled by --no-sound command-line option') else
  if not TryBeginAL(false) then
   Writeln('OpenAL initialization failed : ' +ALActivationErrorMessage +nl+
           'SOUND IS DISABLED') else
  begin
   Writeln('OpenAL initialized, sound enabled');
   InitAL;
  end;

  { init GLWinMessages }
  MessageRectStipple := @ThreeQuartersStipple;

  { init glwindow & loop }
  Glw.OnDraw := Draw;
  Glw.OnInit := Init;
  Glw.OnClose := Close;
  Glw.OnResize := Resize;
  Glw.OnIdle := Idle;
  Glw.OnTimer := Timer;
  Glw.OnKeyDown := KeyDown;
  Glw.AutoRedisplay := true;
  Glw.Caption := 'The Castle';
  Glw.InitLoop;
 finally
  EndAL;
  FreeAndNil(BaseScene);
 end;
end.

{
  Local Variables:
  kam-compile-release-command-win32: "clean_glwindow_unit; fpcrelease"
  kam-compile-release-command-unix: "clean_glwindow_unit; fpcreleaseb -dGLWINDOW_XLIB"
  End:
}