{
  Copyright 2007-2011 Michalis Kamburelis.

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
unit GameCredits;

interface

uses GLWindow, UIControls, VRMLNodes;

{ Show credits. }
procedure ShowCredits(ControlsUnder: TUIControlList);

{ Although this will be called by Window.Close, it may be too late
  (this must be called before releasing GLContextCache).
  So you should call this explicitly. }
procedure CredistGLContextRelease;

implementation

uses SysUtils, GL, GLU, KambiGLUtils, GLWinMessages,
  GameNotifications, KambiStringUtils, GLWinModes,
  GameInputs, GamePlay, GameWindow,
  GameVideoOptions, VectorMath, VRMLGLScene, KambiFilesUtils,
  GameHelp, KambiUtils, VRMLFields, KambiTimeUtils, KeysMouse;

var
  UserQuit: boolean;
  CreditsModel: TVRMLGLScene;
  AnimationTime: TKamTime;
  AnimationSpeed, AnimationEnd: TKamTime;

procedure Draw(Window: TGLWindow);

  procedure ProjectionPushSet;
  begin
    glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glMultMatrix(PerspectiveProjMatrixDeg(
        ViewAngleDegY, Window.Width / Window.Height,
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

var
  Params: TBasicRenderParams;
begin
  glScissor(25, 20, Window.Width - 25, Window.Height - 20 -  160);
  glEnable(GL_SCISSOR_TEST);

  ProjectionPushSet;
  try
    { We want to use depth buffer in CreditsModel, but don't want to mix
      with background level depth values (as we even use different near/far
      projection values). }
    glClear(GL_DEPTH_BUFFER_BIT);

    glLoadIdentity;
    glTranslatef(0, AnimationSpeed * AnimationTime, 0);

    { TODO: remove need for Params, render CreditsModel as part of scene manager }
    Params := TBasicRenderParams.Create;
    try
      CreditsModel.Render(nil, Params);
      Params.Transparent := true;
      CreditsModel.Render(nil, Params);
    finally FreeAndNil(Params) end;
  finally ProjectionPop end;

  glDisable(GL_SCISSOR_TEST);
end;

procedure Idle(Window: TGLWindow);
begin
  AnimationTime := AnimationTime + Window.Fps.IdleSpeed;
  if AnimationTime > AnimationEnd then
    UserQuit := true;
end;

procedure CloseQuery(Window: TGLWindow);
begin
  MessageOK(Window, 'You can''t exit now.');
end;

procedure KeyDown(Window: TGLWindow; key: TKey; c: char);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(Key, #0, false, mbLeft, mwNone) then
    SaveScreen else
  if C in [CharEscape, CharEnter, ' '] then
    UserQuit := true;
end;

procedure MouseDown(Window: TGLWindow; Button: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, true, Button, mwNone) then
    SaveScreen else
    { any mouse press ends credits }
    UserQuit := true;
end;

procedure MouseWheel(Window: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, false, mbLeft, MouseWheelDirection(Scroll, Vertical)) then
    SaveScreen;
end;

{ $define DEBUG_ALWAYS_RELOAD_CREDITS}

{$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
procedure OpenWindow(Window: TGLWindow); forward;
{$endif}

procedure ShowCredits(ControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
begin
  {$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
  CredistGLContextRelease;
  OpenWindow(Window);
  {$endif}

  AnimationTime := 0;

  SavedMode := TGLMode.CreateReset(Window, 0, false,
    @Draw, nil, @CloseQuery,
    true { FPSActive should not be needed anymore, but I leave it. });
  try
    Window.AutoRedisplay := true; { scrolling text animation }

    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnMouseWheel := @MouseWheel;
    Window.OnIdle := @Idle;
    Window.OnDrawStyle := ds3D;

    UserQuit := false;

    Window.Controls.Add(Notifications);
    Window.Controls.AddList(ControlsUnder);

    repeat
      Application.ProcessMessage(true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure OpenWindow(Window: TGLWindow);
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

  Info := (CreditsModel.RootNode.FindNodeByName(TWorldInfoNode,
    'MainInfo', true) as TWorldInfoNode).FdInfo;
  AnimationSpeed := StrToFloat(Info.Items[1]);
  AnimationEnd := StrToFloat(Info.Items[2]);
end;

procedure CredistGLContextRelease;
begin
  FreeAndNil(CreditsModel);
end;

procedure CloseWindow(Window: TGLWindow);
begin
  CredistGLContextRelease;
end;

initialization
  Window.OnOpenList.Add(@OpenWindow);
  Window.OnCloseList.Add(@CloseWindow);
finalization
end.
