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

{ }
unit GameCredits;

interface

uses CastleWindow, UIControls, X3DNodes;

{ Show credits. }
procedure ShowCredits(ControlsUnder: TUIControlList);

{ Although this will be called by Window.Close, it may be too late
  (this must be called before releasing GLContextCache).
  So you should call this explicitly. }
procedure CredistGLContextRelease;

implementation

uses SysUtils, GL, GLU, CastleGLUtils, CastleMessages,
  CastleGameNotifications, CastleStringUtils, WindowModes,
  GameInputs, GamePlay, CastleGameCache, GameWindow,
  GameVideoOptions, VectorMath, CastleScene, CastleFilesUtils,
  GameHelp, CastleUtils, X3DFields, CastleTimeUtils, KeysMouse,
  Frustum;

var
  UserQuit: boolean;
  CreditsModel: TCastleScene;
  AnimationTime: TFloatTime;
  AnimationSpeed, AnimationEnd: TFloatTime;

procedure Draw(Window: TCastleWindowBase);
var
  ModelviewMatrix, ProjectionMatrix: TMatrix4Single;

  procedure ProjectionPushSet;
  begin
    glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      ProjectionMatrix := PerspectiveProjMatrixDeg(
        ViewAngleDegY, Window.Width / Window.Height,
        { constant near / far here is Ok, since I render known geometry }
        0.1, 100);
      glLoadMatrix(ProjectionMatrix);
    glMatrixMode(GL_MODELVIEW);
  end;

  procedure ProjectionPop;
  begin
    glMatrixMode(GL_PROJECTION);
      glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
  end;

var
  Frustum: TFrustum;
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
    ModelviewMatrix := TranslationMatrix(0, AnimationSpeed * AnimationTime, 0);
    glLoadMatrix(ModelviewMatrix);

    { TODO: remove need for Frustum, render CreditsModel as part of scene manager }
    Frustum.Init(ProjectionMatrix, ModelviewMatrix);

    { TODO: remove need for Params, render CreditsModel as part of scene manager }
    Params := TBasicRenderParams.Create;
    try
      Params.Transparent := false; Params.ShadowVolumesReceivers := false; CreditsModel.Render(nil, Frustum, Params);
      Params.Transparent := false; Params.ShadowVolumesReceivers := true ; CreditsModel.Render(nil, Frustum, Params);
      Params.Transparent := true ; Params.ShadowVolumesReceivers := false; CreditsModel.Render(nil, Frustum, Params);
      Params.Transparent := true ; Params.ShadowVolumesReceivers := true ; CreditsModel.Render(nil, Frustum, Params);
    finally FreeAndNil(Params) end;
  finally ProjectionPop end;

  glDisable(GL_SCISSOR_TEST);
end;

procedure Idle(Window: TCastleWindowBase);
begin
  AnimationTime := AnimationTime + Window.Fps.IdleSpeed;
  if AnimationTime > AnimationEnd then
    UserQuit := true;
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  MessageOK(Window, 'You can''t exit now.');
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(Key, #0, false, mbLeft, mwNone) then
    SaveScreen else
  if C in [CharEscape, CharEnter, ' '] then
    UserQuit := true;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, true, Button, mwNone) then
    SaveScreen else
    { any mouse press ends credits }
    UserQuit := true;
end;

procedure MouseWheel(Window: TCastleWindowBase; const Scroll: Single; const Vertical: boolean);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(K_None, #0, false, mbLeft, MouseWheelDirection(Scroll, Vertical)) then
    SaveScreen;
end;

{ $define DEBUG_ALWAYS_RELOAD_CREDITS}

{$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
procedure WindowOpen(const Container: IUIContainer); forward;
{$endif}

procedure ShowCredits(ControlsUnder: TUIControlList);
var
  SavedMode: TGLMode;
begin
  {$ifdef DEBUG_ALWAYS_RELOAD_CREDITS}
  CredistGLContextRelease;
  WindowOpen(Window);
  {$endif}

  AnimationTime := 0;

  SavedMode := TGLMode.CreateReset(Window, 0, false, @Draw, nil, @CloseQuery);
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
      Application.ProcessMessage(true, true);
    until UserQuit;

  finally FreeAndNil(SavedMode); end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(const Container: IUIContainer);
var
  VRMLContents: string;
  Info: TMFString;
begin
  VRMLContents := FileToString(ProgramDataPath + 'data' + PathDelim +
    'menu_bg' + PathDelim + 'credits.wrl');
  StringReplaceAllTo1st(VRMLContents, '$SCastleVersion', SCastleVersion);
  StringReplaceAllTo1st(VRMLContents, '$SCastleWWW', 'WWW: ' + CastleURL);
  StringReplaceAllTo1st(VRMLContents, '$SCompilerDescription', SCompilerDescription);

  CreditsModel := TCastleScene.CreateCustomCache(nil, GLContextCache);
  CreditsModel.Load(LoadX3DClassicFromString(VRMLContents, ''), true);

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

procedure WindowClose(const Container: IUIContainer);
begin
  CredistGLContextRelease;
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
end.
