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

uses CastleWindow, UIControls, X3DNodes, CastleSceneManager;

{ Show credits. }
procedure ShowCredits(ControlsUnder: TUIControlList;
  SceneManagerUnder: TCastleSceneManager);

{ Although this will be called by Window.Close, it may be too late
  (this must be called before releasing GLContextCache).
  So you should call this explicitly. }
procedure CreditsGLContextRelease;

implementation

uses SysUtils, GL, GLU, CastleGLUtils, CastleMessages,
  CastleGameNotifications, CastleStringUtils, WindowModes,
  GamePlay, CastleGameCache, GameWindow,
  GameVideoOptions, VectorMath, CastleScene, CastleFilesUtils,
  GameHelp, CastleUtils, X3DFields, CastleTimeUtils, KeysMouse, Base3D, Classes;

var
  UserQuit: boolean;

{ TCredits ------------------------------------------------------------------- }

type
  TCredits = class(T3DTransform)
  public
    AnimationTime, AnimationSpeed, AnimationEnd: TFloatTime;
    Scene: TCastleScene;
    constructor Create(AOwner: TComponent); override;
    procedure Idle(const CompSpeed: Single; var RemoveMe: TRemoveType); override;
  end;

constructor TCredits.Create(AOwner: TComponent);
var
  VRMLContents: string;
  Info: TMFString;
begin
  inherited;

  VRMLContents := FileToString(ProgramDataPath + 'data' + PathDelim +
    'menu_bg' + PathDelim + 'credits.wrl');
  StringReplaceAllTo1st(VRMLContents, '$SCastleVersion', SCastleVersion);
  StringReplaceAllTo1st(VRMLContents, '$SCastleWWW', 'WWW: ' + CastleURL);
  StringReplaceAllTo1st(VRMLContents, '$SCompilerDescription', SCompilerDescription);

  Scene := TCastleScene.CreateCustomCache(Self, GLContextCache);
  Scene.Load(LoadX3DClassicFromString(VRMLContents, ''), true);

  Add(Scene);

  Info := (Scene.RootNode.FindNodeByName(TWorldInfoNode,
    'MainInfo', true) as TWorldInfoNode).FdInfo;
  AnimationSpeed := StrToFloat(Info.Items[1]);
  AnimationEnd := StrToFloat(Info.Items[2]);
end;

procedure TCredits.Idle(const CompSpeed: Single; var RemoveMe: TRemoveType);
begin
  AnimationTime := AnimationTime + CompSpeed;
  Translation := Vector3Single(0, AnimationSpeed * AnimationTime, 0);
  if AnimationTime > AnimationEnd then
    UserQuit := true;
end;

{ others --------------------------------------------------------------------- }

var
  Credits: TCredits;
  CreditsSceneManager: TCastleSceneManager;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  MessageOK(Window, 'You can''t exit now.');
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  if C in [CharEscape, CharEnter, ' '] then
    UserQuit := true;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
begin
  { any mouse press ends credits }
  UserQuit := true;
end;

procedure ShowCredits(ControlsUnder: TUIControlList;
  SceneManagerUnder: TCastleSceneManager);
var
  SavedMode: TGLMode;
begin
  SavedMode := TGLMode.CreateReset(Window, 0, false, nil, nil, @CloseQuery);
  try
    Window.AutoRedisplay := true; { scrolling text animation }

    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;

    UserQuit := false;
    Credits.AnimationTime := 0;

    Window.Controls.Add(Notifications);
    Window.Controls.AddList(ControlsUnder);

    Window.Controls.Insert(0, CreditsSceneManager);

    repeat
      Application.ProcessMessage(true, true);
    until UserQuit;
  finally FreeAndNil(SavedMode) end;
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(const Container: IUIContainer);
begin
  Credits := TCredits.Create(nil);

  { We want to create separate scene manager for credits display because:
    - we want it displayed always on top (so depth buffer should be cleared)
      of the background,
    - ignoring UseGlobalLights of the background level.
    - with own projection, regardles of the background level projection.
    - with own size. }
  CreditsSceneManager := TCastleSceneManager.Create(nil);
  CreditsSceneManager.FullSize := false;
  CreditsSceneManager.Left := 25;
  CreditsSceneManager.Bottom := 20;
  CreditsSceneManager.Width := Window.Width - CreditsSceneManager.Left * 2;
  CreditsSceneManager.Height := Window.Height - CreditsSceneManager.Bottom * 2 - 160;
  CreditsSceneManager.Transparent := true;

  CreditsSceneManager.Items.Add(Credits);
  CreditsSceneManager.MainScene := Credits.Scene;
end;

procedure CreditsGLContextRelease;
begin
  FreeAndNil(Credits);
  FreeAndNil(CreditsSceneManager);
end;

procedure WindowClose(const Container: IUIContainer);
begin
  CreditsGLContextRelease;
end;

initialization
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
end.
