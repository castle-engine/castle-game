{
  Copyright 2007-2017 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit GameCredits;

interface

uses Classes,
  CastleWindow, CastleUIControls, X3DNodes, CastleSceneManager, CastleCameras,
  CastleUIState, CastleTransform, CastleTimeUtils, CastleScene, CastleKeysMouse;

type
  TStateCredits = class(TUIState)
  strict private
    type
      T3DCredits = class(TCastleTransform)
      public
        AnimationTime, AnimationSpeed, AnimationEnd: TFloatTime;
        Scene: TCastleScene;
        constructor Create(AOwner: TComponent); override;
        procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
      end;
    var
    Credits: T3DCredits;
    CreditsSceneManager: TCastleSceneManager;
  public
    ControlsUnder: TCastleUserInterface;
    SceneManagerUnder: TCastleSceneManager;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateCredits: TStateCredits;

implementation

uses SysUtils, Math,
  CastleGLUtils, CastleMessages,
  CastleGameNotifications, CastleStringUtils,
  CastleApplicationProperties, CastleUtils, X3DFields,
  CastleVectors, CastleFilesUtils, X3DLoad,
  GamePlay, GameWindow, GameVideoOptions, GameHelp;

function LoadX3DClassicFromString(const FileContents: string;
  const BaseUrl: string): TX3DRootNode;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(FileContents);
  try
    Result := LoadNode(Stream, BaseUrl, 'model/x3d+vrml');
  finally FreeAndNil(Stream) end;
end;

{ T3DCredits ------------------------------------------------------------------- }

constructor TStateCredits.T3DCredits.Create(AOwner: TComponent);
var
  VRMLContents: string;
  Info: TMFString;
begin
  inherited;

  VRMLContents := FileToString(ApplicationData('menu_bg/credits.wrl'));
  StringReplaceAllVar(VRMLContents, '$SCastleVersion', SCastleVersion);
  StringReplaceAllVar(VRMLContents, '$SCastleWWW', 'WWW: ' + CastleURL);
  StringReplaceAllVar(VRMLContents, '$SCompilerDescription', SCompilerDescription);

  Scene := TCastleScene.Create(Self);
  Scene.Load(LoadX3DClassicFromString(VRMLContents, ''), true);

  Add(Scene);

  Info := (Scene.RootNode.FindNodeByName(TWorldInfoNode,
    'MainInfo', true) as TWorldInfoNode).FdInfo;
  AnimationSpeed := StrToFloat(Info.Items[1]);
  AnimationEnd := StrToFloat(Info.Items[2]);
end;

procedure TStateCredits.T3DCredits.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  AnimationTime := AnimationTime + SecondsPassed;
  Translation := Vector3(0, AnimationSpeed * AnimationTime, 0);
  if AnimationTime > AnimationEnd then
    TUIState.Pop(StateCredits);
end;

{ TStateCredits -------------------------------------------------------------- }

constructor TStateCredits.Create(AOwner: TComponent);
begin
  inherited;

  Credits := T3DCredits.Create(nil);

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
  CreditsSceneManager.Width := Max(0, Window.Width - CreditsSceneManager.Left * 2);
  CreditsSceneManager.Height := Max(0, Window.Height - CreditsSceneManager.Bottom * 2 - 160);
  CreditsSceneManager.Transparent := true;

  CreditsSceneManager.Items.Add(Credits);
  CreditsSceneManager.Items.MainScene := Credits.Scene;

  CreditsSceneManager.NavigationType := ntNone;
end;

destructor TStateCredits.Destroy;
begin
  FreeAndNil(Credits);
  FreeAndNil(CreditsSceneManager);
  inherited;
end;

function TStateCredits.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(CharEscape) or
     Event.IsKey(CharEnter) or
     Event.IsKey(' ') or
     { any mouse press ends credits }
     (Event.EventType = itMouseButton) then
  begin
    TUIState.Pop(StateCredits);
    Result := true;
  end;
end;

procedure TStateCredits.Start;
begin
  inherited;
  Credits.AnimationTime := 0;
  Credits.Translation := TVector3.Zero;

  InsertBack(ControlsUnder);
  InsertFront(CreditsSceneManager);
end;

procedure TStateCredits.Stop;
begin
  RemoveControl(ControlsUnder);
  RemoveControl(CreditsSceneManager);
  inherited;
end;

end.
