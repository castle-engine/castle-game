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

procedure PlayLevel(ALevel: TLevel; APlayer: TPlayer);

var
  { Currently used player by PlayLevel. nil if PlayLevel doesn't work
    right now. }
  Player: TPlayer;

  { Currently used level by PlayLevel. nil if PlayLevel doesn't work
    right now. }
  Level: TLevel;

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

procedure LevelFinished(NextLevel: TLevel);

implementation

uses Math, SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath, Boxes3d, TimeMessages, Images,
  CastleHelp, OpenGLFonts, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit,
  CastleItems, VRMLTriangleOctree, RaysWindow;

var
  GameEnded: boolean;
  GameMessagesManager: TTimeMessagesManager;
  GLList_Draw2dBegin: TGLuint;
  GLList_BlankIndicatorImage: TGLuint;
  GLList_RedIndicatorImage: TGLuint;
  GLList_BlueIndicatorImage: TGLuint;
  GLList_InventorySlot: TGLuint;
  InventoryVisible: boolean;
  InventoryNamesFont: TGLBitmapFont_Abstract;
  InventoryCurrentItem: Integer;

const
  ViewAngleDegX = 45.0;

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX,
    Glw.Height / Glw.Width);
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

  ItemSlotX = RequiredScreenWidth - InventorySlotWidth;

  function ItemSlotY(I: Integer): Integer;
  begin
    Result := InventorySlotHeight * (MaxInventorySlotsVisible - 1 - I);
  end;

var
  PlayerLifeMapped, I, X, Y, MaxItemShown: Integer;
  S: string;
begin
  if Player.EquippedWeapon <> nil then
    glCallList((Player.EquippedWeapon.Kind as TItemWeaponKind).
      GLList_DrawScreenImage);

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
        X := ItemSlotX;
        Y := ItemSlotY(I);

        glRasterPos2i(X, Y);
        glCallList(GLList_InventorySlot);
      end;
    glDisable(GL_BLEND);

    MaxItemShown := Min(MaxInventorySlotsVisible - 1, Player.Items.Count - 1);

    glAlphaFunc(GL_GREATER, 0.5);
    glEnable(GL_ALPHA_TEST);
      for I := 0 to MaxItemShown do
      begin
        X := ItemSlotX;
        Y := ItemSlotY(I);

        glRasterPos2i(X + InventorySlotMargin, Y + InventorySlotMargin);
        glCallList(Player.Items[I].Kind.GLList_DrawImage);
      end;
    glDisable(GL_ALPHA_TEST);

    if Between(InventoryCurrentItem, 0, Player.Items.Count - 1) and
       (InventoryCurrentItem <= MaxItemShown) then
    begin
      glColor4f(0.8, 0.8, 0.8, 1);
      DrawGLRectBorder(
        ItemSlotX + InventorySlotMargin,
        ItemSlotY(InventoryCurrentItem) + InventorySlotMargin,
        ItemSlotX + InventorySlotWidth - InventorySlotMargin,
        ItemSlotY(InventoryCurrentItem)
          + InventorySlotHeight - InventorySlotMargin);
    end;

    glColor4f(1, 1, 0.5, 1);
    for I := 0 to MaxItemShown do
    begin
      X := ItemSlotX;
      Y := ItemSlotY(I);

      glRasterPos2i(X + InventorySlotMargin, Y + InventorySlotMargin);

      S := Player.Items[I].Kind.Name;
      if Player.Items[I].Quantity <> 1 then
        S += ' (' + IntToStr(Player.Items[I].Quantity) + ')';
      InventoryNamesFont.Print(S);
    end;
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

  Level.Render(Player.Navigator.Frustum);

  Level.Items.Render(Player.Navigator.Frustum);

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
  Level.Idle(Glw.FpsCompSpeed);
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

  procedure ChangeInventoryCurrentItem(Change: Integer);
  begin
    if Player.Items.Count = 0 then
      InventoryCurrentItem := -1 else
    if InventoryCurrentItem >= Player.Items.Count then
      InventoryCurrentItem := Player.Items.Count - 1 else
    if InventoryCurrentItem < 0 then
      InventoryCurrentItem := 0 else
      InventoryCurrentItem := ChangeIntCycle(
        InventoryCurrentItem, Change, Player.Items.Count - 1);
  end;

  procedure DropItem;
  var
    DropppedItem: TItem;
    DropPosition, PushVector: TVector3Single;
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround: Single;
    PushVectorLength: Single;
  begin
    if Between(InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      DropppedItem := Player.DropItem(InventoryCurrentItem);
      if DropppedItem <> nil then
      begin
        { Calculate DropPosition.
          TODO: this should be done better,
          DropPosition should be always initialized to Player.Navigator.CameraPos
          and then we should just let items to fall down on the ground. }
        Player.Navigator.DoGetCameraHeight(IsAboveTheGround,
          SqrHeightAboveTheGround);

        if not IsAboveTheGround then
          DropPosition := Player.Navigator.CameraPos else
          DropPosition := VectorSubtract(Player.Navigator.CameraPos,
            VectorAdjustToLength(Player.Navigator.HomeCameraUp,
              Sqrt(SqrHeightAboveTheGround)));

        { We must move the item a little before us to
          1. show visually player that the item was dropped
          2. to avoid automatically picking it again

          Note that I take PushVector from CameraDirInHomePlane,
          not from CameraDir, otherwise when player is looking
          down he could be able to put item "inside the ground".
          TODO: actually, I should just check collision of item with level,
          to avoid "pushing item into the wall". }
        PushVector := Player.Navigator.CameraDirInHomePlane;
        PushVectorLength := Max(
          Player.Navigator.RealCameraPreferredHeight,
          Box3dSizeX(DropppedItem.Kind.BoundingBoxRotated) * 2,
          Box3dSizeY(DropppedItem.Kind.BoundingBoxRotated) * 2);
        VectorAdjustToLengthTo1st(PushVector, PushVectorLength);
        VectorAddTo1st(DropPosition, PushVector);

        Level.Items.Add(TItemOnLevel.Create(DropppedItem, DropPosition));
      end;
    end else
      GameMessage('Nothing to drop - select some item first');
  end;

  procedure UseItem;
  var
    UsedItem: TItem;
    UsedItemIndex: Integer;
  begin
    if Between(InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      UsedItem := Player.Items[InventoryCurrentItem];
      UsedItem.Kind.Use(UsedItem);
      if UsedItem.Quantity = 0 then
      begin
        { Note that I don't delete here using
          Player.Items.Delete(InventoryCurrentItem);,
          because indexes on Player.Items could change because
          of TItemKind.Use call. }
        UsedItemIndex := Player.Items.IndexOf(UsedItem);
        if UsedItemIndex <> -1 then
          Player.DeleteItem(UsedItemIndex).Free;
      end;
    end else
      GameMessage('Nothing to use - select some item first');
  end;

  procedure GameCancel;
  begin
    if MessageYesNo(Glw, 'Are you sure you want to end the game ?', taLeft) then
      GameEnded := true;
  end;

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
        'l': Player.Life := Min(Player.Life + 10, Player.MaxLife);
        'L': Player.Life := Max(Player.Life - 10, 0);

        'i': InventoryVisible := not InventoryVisible;
        '[': ChangeInventoryCurrentItem(-1);
        ']': ChangeInventoryCurrentItem(+1);
        'd': DropItem;
        CharEnter: UseItem;

        CharEscape: GameCancel;
      end;
  end;
end;

procedure MouseDown(Glwin: TGLWindow; Btn: TMouseButton);
type
  { TODO: This will be expanded with at least poEnemy in the future. }
  TPickedObjectType = (poNone, poLevel, poItem);
var
  Ray0, RayVector: TVector3Single;
  LevelCollisionIndex, ItemCollisionIndex, I: integer;
  IntersectionDistance, ThisIntersectionDistance: Single;
  PickedObjectType: TPickedObjectType;
begin
  if Btn = mbLeft then
  begin
    Glw.MousePickedRay(ViewAngleDegX, ViewAngleDegY, Ray0, RayVector);

    { Picking is not an often called procedure, so I can freely normalize
      here to get exact distance to picked object in IntersectionDistance. }
    NormalizeTo1st(RayVector);

    IntersectionDistance := MaxSingle;
    PickedObjectType := poNone;

    { Now start picking, by doing various tests for collisions between
      Ray0 and RayVector. The pick that has smallest IntersectionDistance
      "wins". }

    { Collision with Level.Scene }
    LevelCollisionIndex := Level.Scene.DefaultTriangleOctree.RayCollision(
      ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex, false);
    if (LevelCollisionIndex <> NoItemIndex) and
       ( (PickedObjectType = poNone) or
         (ThisIntersectionDistance < IntersectionDistance) ) then
    begin
      PickedObjectType := poLevel;
      IntersectionDistance := ThisIntersectionDistance;
    end;

    { Collision with Level.Items }
    for I := 0 to Level.Items.Count - 1 do
      if TryBoxRayClosestIntersection(ThisIntersectionDistance,
           Level.Items[I].BoundingBox, Ray0, RayVector) and
         ( (PickedObjectType = poNone) or
           (ThisIntersectionDistance < IntersectionDistance)
         ) then
      begin
        ItemCollisionIndex := I;
        PickedObjectType := poItem;
        IntersectionDistance := ThisIntersectionDistance;
      end;

    { End. Now call appropriate picked notifier. }
    case PickedObjectType of
      poLevel:
        begin
          Level.TrianglePicked(IntersectionDistance,
            Level.Scene.DefaultTriangleOctree.OctreeItems.
              Items[LevelCollisionIndex]);
        end;
      poItem:
        begin
          Level.Items[ItemCollisionIndex].ItemPicked(IntersectionDistance);
        end;
    end;
  end;
end;

function MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; var NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Level.MoveAllowed(Navigator,
    ProposedNewPos, NewPos, BecauseOfGravity);
end;

procedure GetCameraHeight(Navigator: TMatrixNavigator;
  var IsAboveTheGround: boolean; var SqrHeightAboveTheGround: Single);
begin
  Level.GetCameraHeight(Navigator,
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

procedure PlayLevel(ALevel: TLevel; APlayer: TPlayer);
const
  HeadlightPower = 0.5;
var
  SavedMode: TGLMode;
  CamPos, CamDir, CamUp: TVector3Single;
begin
  Level := ALevel;
  Player := APlayer;
  InventoryVisible := false;
  InventoryCurrentItem := 0;
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
        Glw.OnMouseDown := MouseDown;

        Glw.EventResize;

        GameEnded := false;

        glEnable(GL_LIGHTING);
        if Level.Headlight then
        begin
          glEnable(GL_LIGHT0);
          glLightv(GL_LIGHT0, GL_DIFFUSE,
            Vector4Single(HeadlightPower, HeadlightPower, HeadlightPower, 1));
          glLightv(GL_LIGHT0, GL_SPECULAR,
            Vector4Single(HeadlightPower, HeadlightPower, HeadlightPower, 1));
        end;

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
            until GameEnded;
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

procedure LevelFinished(NextLevel: TLevel);
begin
  if NextLevel = nil then
  begin
    MessageOK(Glw, 'Level finished', taLeft);
    GameEnded := true;
  end else
  begin
    MessageOK(Glw, 'Next level: TODO');
    GameEnded := true;
  end;
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