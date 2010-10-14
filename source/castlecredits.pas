{
  Copyright 2007-2010 Michalis Kamburelis.

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
unit CastleCredits;

interface

uses GLWindow, UIControls;

{ Show credits. }
procedure ShowCredits(ControlsUnder: TUIControlList);

{ Although this will be called by Glw.Close, it may be too late
  (this must be called before releasing GLContextCache).
  So you should call this explicitly. }
procedure CredistGLContextRelease;

implementation

uses SysUtils, GL, GLU, KambiGLUtils, GLWinMessages,
  CastleTimeMessages, KambiStringUtils, GLWinModes,
  CastleInputs, CastlePlay, CastleWindow,
  CastleVideoOptions, VectorMath, VRMLGLScene, KambiFilesUtils,
  CastleHelp, KambiUtils, VRMLNodes, VRMLFields, KambiTimeUtils, KeysMouse;

var
  UserQuit: boolean;
  CreditsModel: TVRMLGLScene;
  AnimationTime: TKamTime;
  AnimationSpeed, AnimationEnd: TKamTime;

procedure Draw(Glwin: TGLWindow);

  procedure ProjectionPushSet;
  begin
    glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glMultMatrix(PerspectiveProjMatrixDeg(
        ViewAngleDegY, Glwin.Width / Glwin.Height,
        { constant near / far here is Ok, since I render known geometry }
        0.1, 100));
    glMatrixMode(GL_MODELVIEW);
  end;

  procedure ProjectionPop;
  begin
    glMatrixMode(GL_PROJECTION);
      glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;

begin
  glScissor(25, 20, Glw.Width - 25, Glw.Height - 20 -  160);
  glEnable(GL_SCISSOR_TEST);

  ProjectionPushSet;
  try
    { We want to use depth buffer in CreditsModel, but don't want to mix
      with background level depth values (as we even use different near/far
      projection values). }
    glClear(GL_DEPTH_BUFFER_BIT);

    glLoadIdentity;
    glTranslatef(0, AnimationSpeed * AnimationTime, 0);

    CreditsModel.Render(nil, tgAll);
  finally ProjectionPop end;

  glDisable(GL_SCISSOR_TEST);
end;

procedure Idle(Glwin: TGLWindow);
begin
  AnimationTime := AnimationTime + Glwin.Fps.IdleSpeed;
  if AnimationTime > AnimationEnd then
    UserQuit := true;

  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(Key, #0, false, mbLeft, mwNone) then
    SaveScreen else
  if C in [CharEscape, CharEnter, ' '] then
    UserQuit := true;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, true, Button, mwNone) then
    SaveScreen else
    { any mouse press ends credits }
    UserQuit := true;
end;

procedure MouseWheel(Glwin: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, false, mbLeft, MouseWheelDirection(Scroll, Vertical)) then
    SaveScreen;
end;

{ $define DEBUG_ALWAYS_RELOAD_CREDITS}

{$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
procedure InitGLW(Glwin: TGLWindow); forward;
{$endif}

procedure ShowCredits(ControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
begin
  {$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
  CredistGLContextRelease;
  InitGLW(Glw);
  {$endif}

  AnimationTime := 0;

  SavedMode := TGLMode.CreateReset(glw, 0, false,
    @Draw, nil, @CloseQuery,
    true { FPSActive should not be needed anymore, but I leave it. });
  try
    Glw.AutoRedisplay := true; { scrolling text animation }

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnMouseWheel := @MouseWheel;
    Glw.OnIdle := @Idle;
    Glw.OnDrawStyle := ds3D;

    UserQuit := false;

    Glw.Controls.AddList(ControlsUnder);

    repeat
      Application.ProcessMessage(true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure InitGLW(Glwin: TGLWindow);
var
  VRMLContents: string;
  Info: TMFString;
begin
  VRMLContents := FileToString(ProgramDataPath + 'data' + PathDelim +
    'menu_bg' + PathDelim + 'credits.wrl');
  StringReplaceAllTo1st(VRMLContents, '$SCastleVersion', SCastleVersion);
  StringReplaceAllTo1st(VRMLContents, '$SCastleWWW', SCastleWWW);
  StringReplaceAllTo1st(VRMLContents, '$SCompilerDescription', SCompilerDescription);

  CreditsModel := TVRMLGLScene.CreateCustomCache(nil, GLContextCache);
  CreditsModel.Load(LoadVRMLClassicFromString(VRMLContents, ''), true);

  AttributesSet(CreditsModel.Attributes, btIncrease);
  CreditsModel.Attributes.UseSceneLights := true;

  CreditsModel.PrepareResources([tgAll], [prRender, prBoundingBox], false);

  Info := (CreditsModel.RootNode.FindNodeByName(TNodeWorldInfo,
    'MainInfo', true) as TNodeWorldInfo).FdInfo;
  AnimationSpeed := StrToFloat(Info.Items.Items[1]);
  AnimationEnd := StrToFloat(Info.Items.Items[2]);
end;

procedure CredistGLContextRelease;
begin
  FreeAndNil(CreditsModel);
end;

procedure CloseGLW(Glwin: TGLWindow);
begin
  CredistGLContextRelease;
end;

initialization
  Glw.OnInitList.Add(@InitGLW);
  Glw.OnCloseList.Add(@CloseGLW);
finalization
end.
