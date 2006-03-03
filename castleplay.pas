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

uses Classes, CastleLevel, CastlePlayer;

procedure PlayLevel(ALevel: TCastleLevel; APlayer: TPlayer);

var
  { Currently used player by PlayLevel. nil if PlayLevel doesn't work
    right now. }
  Player: TPlayer;

  { Currently used level by PlayLevel. nil if PlayLevel doesn't work
    right now. }
  Level: TCastleLevel;

  { These are all messages passed to GameMessage.
    Created / destroyed in this unit's initialization / finalization.
    They are not broken (to fit into some particular line width).

    You should clear this when new game starts or load this from file
    when loading saved game.
    When PlayLevel runs, you cannot modify it directly, you can change it
    only by calling GameMessage. }
  GameMessages: TStringList;

{ Add message to GameMessages and (only if PlayLevel is running)
  display it on the game screen. }
procedure GameMessage(const S: string);

implementation

uses SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath, Boxes3d, TimeMessages, Images,
  CastleHelp, OpenGLFonts, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit;

var
  GameCancelled: boolean;
  GameMessagesManager: TTimeMessagesManager;
  GLList_Draw2dBegin: TGLuint;
  GLList_BlankIndicatorImage: TGLuint;
  GLList_RedIndicatorImage: TGLuint;
  GLList_BlueIndicatorImage: TGLuint;
  GLList_InventorySlot: TGLuint;
  InventoryVisible: boolean;
  InventoryNamesFont: TGLBitmapFont_Abstract;

const
  ViewAngleDegX = 45.0;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX, Glw.Height / Glw.Width);
end;

{ If ALActive then update listener POSITION and ORIENTATION
  and GAIN based on Player.Navigator.Camera* }
procedure alUpdateListener;
begin
  if ALActive then
  begin
    alListenerVector3f(AL_POSITION, Player.Navigator.CameraPos);
    alListenerOrientation(Player.Navigator.CameraDir, Player.Navigator.CameraUp);
  end;
end;

procedure Resize(Glwin: TGLWindow);

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
    glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
    Player.Navigator.ProjectionMatrix := ProjectionMatrix;
  end;

begin
  { update glViewport and projection }
  glViewport(0, 0, Glwin.Width, Glwin.Height);
  ProjectionGLPerspective(ViewAngleDegY, Glwin.Width / Glwin.Height,
    Level.ProjectionNear, Level.ProjectionFar);

  UpdateNavigatorProjectionMatrix;
end;

procedure Draw2D(Draw2DData: Integer);
const
  IndicatorHeight = 120;
  IndicatorMargin = 5;

  InventorySlotWidth = 100;
  InventorySlotHeight = 100;
  InventorySlotMargin = 2;
  MaxInventorySlotsVisible = RequiredScreenHeight div InventorySlotHeight;

var
  PlayerLifeMapped, I, X, Y: Integer;
  S: string;
begin
  glCallList(GLList_Draw2dBegin);

  GameMessagesManager.Draw2d(RequiredScreenWidth, RequiredScreenHeight,
    Glw.Width, Glw.Height);

  glRasterPos2i(IndicatorMargin, IndicatorMargin);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

    PlayerLifeMapped :=
      Round(MapRange(Player.Life, 0, Player.MaxLife, 0, IndicatorHeight));

    { Note that Player.Life may be > Player.MaxLife, and
      Player.Life may be < 0. }
    if PlayerLifeMapped >= IndicatorHeight then
      glCallList(GLList_RedIndicatorImage) else
    if PlayerLifeMapped < 0 then
      glCallList(GLList_BlankIndicatorImage) else
    begin
      glEnable(GL_SCISSOR_TEST);
        glScissor(IndicatorMargin, IndicatorMargin, RequiredScreenWidth, PlayerLifeMapped);
        glCallList(GLList_RedIndicatorImage);
        glScissor(IndicatorMargin, IndicatorMargin + PlayerLifeMapped,
          RequiredScreenWidth, RequiredScreenHeight);
        glCallList(GLList_BlankIndicatorImage);
      glDisable(GL_SCISSOR_TEST);
    end;
  glDisable(GL_BLEND);

  if InventoryVisible then
  begin
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      for I := 0 to MaxInventorySlotsVisible - 1 do
      begin
        X := RequiredScreenWidth - InventorySlotWidth;
        Y := InventorySlotHeight * (MaxInventorySlotsVisible - 1 - I);

        glRasterPos2i(X, Y);
        glCallList(GLList_InventorySlot);
      end;
    glDisable(GL_BLEND);

    glAlphaFunc(GL_GREATER, 0.5);
    glEnable(GL_ALPHA_TEST);
      for I := 0 to Min(MaxInventorySlotsVisible - 1, Player.Items.Count - 1) do
      begin
        X := RequiredScreenWidth - InventorySlotWidth;
        Y := InventorySlotHeight * (MaxInventorySlotsVisible - 1 - I);

        glRasterPos2i(X + InventorySlotMargin, Y + InventorySlotMargin);
        glCallList(Player.Items[I].Kind.GLList_DrawImage);

        S := Player.Items[I].Kind.Name;
        if Player.Items[I].Quantity <> 1 then
          S += ' (' + IntToStr(Player.Items[I].Quantity) + ')';
        InventoryNamesFont.Print(S);
      end;
    glDisable(GL_ALPHA_TEST);
  end;
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

  Level.Scene.RenderFrustumOctree(Player.Navigator.Frustum);

  Level.Items.Render;

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST); { not needed now, but in the future will be needed }
    ProjectionGLPushPop(Draw2d, 0, Ortho2dProjMatrix(
      0, RequiredScreenWidth, 0, RequiredScreenHeight));
  glPopAttrib;
end;

procedure Idle(Glwin: TGLWindow);
var
  PickItemIndex: Integer;
begin
  GameMessagesManager.Idle;
  Level.Items.Idle(Glw.FpsCompSpeed);

  PickItemIndex := Level.Items.PlayerCollision;
  if PickItemIndex <> -1 then
  begin
    Player.PickItem(Level.Items[PickItemIndex].ExtractItem);
    Level.Items.FreeAndNil(PickItemIndex);
    Level.Items.Delete(PickItemIndex);
  end;
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
        { TODO --- this is just for test, in real game this shouldn't
          be so easy to enter FlyingMode (should require some item, spell etc.) }
        'f': Player.FlyingMode := not Player.FlyingMode;

        { TODO: some button visible in player's window to access this should be
          visible. }
        'm': ViewGameMessages;

        { TODO: just for test: }
        'l': Player.Life := Player.Life + 10;
        'L': Player.Life := Player.Life - 10;

        'i': InventoryVisible := not InventoryVisible;
        CharEscape: GameCancelled := true;
      end;
  end;
end;

function MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result :=
    Box3dPointInside(ProposedNewPos, Level.LevelBox) and
    Level.Scene.DefaultTriangleOctree.MoveAllowed(
      Navigator.CameraPos, ProposedNewPos, NewPos, Level.CameraRadius);
end;

procedure GetCameraHeight(Navigator: TMatrixNavigator;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
begin
  Level.Scene.DefaultTriangleOctree.GetCameraHeight(
    TMatrixWalker(Navigator).CameraPos,
    TMatrixWalker(Navigator).HomeCameraUp,
    IsAboveTheGround, SqrHeightAboveTheGround);
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

procedure PlayLevel(ALevel: TCastleLevel; APlayer: TPlayer);
var
  SavedMode: TGLMode;
  CamPos, CamDir, CamUp: TVector3Single;
begin
  Level := ALevel;
  Player := APlayer;
  InventoryVisible := false;
  try

    SavedMode := TGLMode.Create(glw,
      { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
      GL_ENABLE_BIT);
    try
      { init navigator }
      Glw.Navigator := Player.Navigator;
      try
        { Init Player.Navigator properties }
        Player.Navigator.OnMatrixChanged := TDummy.MatrixChanged;
        Player.Navigator.OnMoveAllowed := MoveAllowed;
        Player.Navigator.OnGetCameraHeight := GetCameraHeight;

        { Init initial camera pos }
        Level.Scene.GetPerspectiveCamera(CamPos, CamDir, CamUp);
        VectorAdjustToLengthTo1st(CamDir, Level.CameraRadius *
          0.4 * { I multiply by 0.4 just to get the same thing
          that view3dscene does at this time. }
          Level.NavigationSpeed);
        Player.Navigator.Init(CamPos, CamDir, CamUp, Level.CameraPreferredHeight,
          0.0 { Level.CameraPreferredHeight is already corrected if necessary,
                so I pass here 0.0 instead of CameraRadius } );

        { tests:
          InfoWrite(Format('%f %f %f %f',
            [VectorLen(Player.Navigator.HomeCameraDir),
            VectorLen(Player.Navigator.CameraDir),
            Player.Navigator.CameraPreferredHeight,
            Player.Navigator.MoveSpeed])); }

        { Note that this sets AutoRedisplay to true. }
        SetStandardGLWindowState(Glw, Draw, nil{TODO CloseQuery}, Resize,
          nil, true, true, false, K_None, #0, true, true);

        Glw.OnIdle := Idle;
        Glw.OnTimer := Timer;
        Glw.OnKeyDown := KeyDown;

        Glw.EventResize;

        GameCancelled := false;

        glEnable(GL_LIGHTING);
        if Level.Headlight then
          glEnable(GL_LIGHT0);

        MessageRectStipple := @ThreeQuartersStipple;
        try
          GameMessagesManager := TTimeMessagesManager.Create(
            Glw, hpMiddle, vpDown, Glw.Width);
          try
            GameMessagesManager.MaxMessagesCount := 4;

            { First GameMessage for this level. }
            GameMessage('Loaded level "' + Level.Title + '"');

            repeat
              Glwm.ProcessMessage(true);
            until GameCancelled;
          finally FreeAndNil(GameMessagesManager) end;

        finally MessageRectStipple := nil; end;
      finally
        Glw.Navigator := nil;
        { Clear some Player.Navigator callbacks. }
        Player.Navigator.OnMatrixChanged := nil;
        Player.Navigator.OnMoveAllowed := nil;
        Player.Navigator.OnGetCameraHeight := nil;
      end;
    finally FreeAndNil(SavedMode); end;

  finally
    { clear global vars, for safety }
    Level := nil;
    Player := nil;
  end;
end;

procedure GameMessage(const S: string);
begin
  if GameMessagesManager <> nil then
    GameMessagesManager.Show(S);
  GameMessages.Insert(0, S);
end;

procedure GLWindowInit(Glwin: TGLWindow);

  function PlayerControlFileName(const BaseName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'player_controls' + PathDelim + BaseName;
  end;

  function LoadPlayerControlToDisplayList(const BaseName: string): TGLuint;
  begin
    Result := LoadImageToDispList(
      PlayerControlFileName(BaseName), [TAlphaImage], [], 0, 0);
  end;

const
  { Note: this constant must be synchronized with
    GameMessagesManager.MaxMessagesCount }
  DarkAreaHeight = 80;

  DarkAreaFadeHeight = 20;
  DarkAreaAlpha = 0.3;
var
  I: Integer;
begin
  { Calculate GLList_Draw2dBegin }
  GLList_Draw2dBegin := glGenLists(1);
  glNewList(GLList_Draw2dBegin, GL_COMPILE);
  try
    glLoadIdentity;
    glRasterPos2i(0, 0);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, DarkAreaAlpha);
      glRecti(0, 0, RequiredScreenWidth, DarkAreaHeight);
      for I := 0 to DarkAreaFadeHeight - 1 do
      begin
        glColor4f(0, 0, 0,
          DarkAreaAlpha * (DarkAreaFadeHeight - 1 - I) / DarkAreaFadeHeight);
        glRecti(0, DarkAreaHeight + I, RequiredScreenWidth, DarkAreaHeight + I + 1);
      end;
    glDisable(GL_BLEND);
  finally glEndList end;

  GLList_InventorySlot := LoadPlayerControlToDisplayList('item_slot.png');

  GLList_BlankIndicatorImage := LoadPlayerControlToDisplayList('blank.png');
  GLList_RedIndicatorImage := LoadPlayerControlToDisplayList('red.png');
  GLList_BlueIndicatorImage := LoadPlayerControlToDisplayList('blue.png');

  InventoryNamesFont := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans_m10);
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  FreeAndNil(InventoryNamesFont);

  glFreeDisplayList(GLList_Draw2dBegin);
  glFreeDisplayList(GLList_InventorySlot);
  glFreeDisplayList(GLList_BlankIndicatorImage);
  glFreeDisplayList(GLList_RedIndicatorImage);
  glFreeDisplayList(GLList_BlueIndicatorImage);
end;

initialization
  GameMessages := TStringList.Create;
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
finalization
  FreeAndNil(GameMessages);
end.