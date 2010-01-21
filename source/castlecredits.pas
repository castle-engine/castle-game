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

{ }
unit CastleCredits;

interface

uses GLWindow;

{ Show credits. }
procedure ShowCredits(ADrawUnderCredits: TDrawFunc;
  AIdleUnderCredits: TGLWindowFunc);

{ Although this will be called by Glw.Close, it may be too late
  (this must be called before releasing GLContextCache).
  So you should call this explicitly. }
procedure CredistGLContextRelease;

implementation

uses SysUtils, GL, GLU, KambiGLUtils, GLWinMessages, CastleTimeMessages,
  CastleInputs, CastlePlay, KambiStringUtils, GLWinModes, CastleWindow,
  CastleVideoOptions, VectorMath, VRMLGLScene, KambiFilesUtils,
  CastleHelp, KambiUtils, VRMLNodes, VRMLFields, KambiTimeUtils;

var
  DrawUnderCredits: TDrawFunc;
  IdleUnderCredits: TGLWindowFunc;
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
  DrawUnderCredits(Glwin);

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

  if Assigned(IdleUnderCredits) then IdleUnderCredits(Glwin);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  MessageOK(Glwin, 'You can''t exit now.');
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(false, Key, #0, mbLeft) then
    SaveScreen else
  if C in [CharEscape, CharEnter, ' '] then
    UserQuit := true;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(true, K_None, #0, Button) then
    SaveScreen else
    { any mouse press ends credits }
    UserQuit := true;
end;

{ $define DEBUG_ALWAYS_RELOAD_CREDITS}

{$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
procedure InitGLW(Glwin: TGLWindow); forward;
{$endif}

procedure ShowCredits(ADrawUnderCredits: TDrawFunc;
  AIdleUnderCredits: TGLWindowFunc);
var
  SavedMode: TGLMode;
begin
  {$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
  CredistGLContextRelease;
  InitGLW(Glw);
  {$endif}

  DrawUnderCredits := ADrawUnderCredits;
  IdleUnderCredits := AIdleUnderCredits;
  AnimationTime := 0;

  SavedMode := TGLMode.Create(glw, 0, false);
  try
    TGLWindowState.SetStandardState(Glw, @Draw, @CloseQuery, Glw.OnResize,
      nil,
      true { AutoRedisplay for background level updates },
      true { FPSActive should not be needed anymore, but I leave it. },
      false, K_None, #0, false, nil);

    Glw.OnKeyDown := @KeyDown;
    Glw.OnMouseDown := @MouseDown;
    Glw.OnIdle := @Idle;

    UserQuit := false;

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
  CreditsModel.Load(ParseVRMLFileFromString(VRMLContents, ''), true);

  AttributesSet(CreditsModel.Attributes, btIncrease);
  CreditsModel.Attributes.UseSceneLights := true;

  CreditsModel.PrepareRender([tgAll], [prBoundingBox], false);

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
