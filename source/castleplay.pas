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

uses Classes, CastleLevel, CastlePlayer, OpenGLFonts;

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

{ Saves a screen, causing also appropriate GameMessage. }
procedure SaveScreen;

{ ViewAngleDegX and ViewAngleDegY specify field of view in the game. }
const
  ViewAngleDegX = 45.0;
function ViewAngleDegY: Single;

var
  { These fonts can be used globally by anything in this game.
    They are initialized in Glw.OnInit and finalized in Glw.OnClose in this unit. }
  Font_BFNT_BitstreamVeraSans_m10: TGLBitmapFont_Abstract;
  Font_BFNT_BitstreamVeraSans: TGLBitmapFont_Abstract;

var
  { Read-only from outside of this unit. }
  GameEnded: boolean;

{ Note that when Player.Dead, confirmation will never be required anyway. }
procedure GameCancel(RequireConfirmation: boolean);

implementation

uses Math, SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath, Boxes3d, TimeMessages, Images,
  CastleHelp, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit,
  BFNT_BitstreamVeraSans_Unit,
  CastleItems, VRMLTriangleOctree, RaysWindow, KambiStringUtils,
  KambiFilesUtils, CastleKeys, CastleGameMenu, CastleSound;

var
  GameMessagesManager: TTimeMessagesManager;
  GLList_Draw2dBegin: TGLuint;

  GLList_InventorySlot: TGLuint;
  InventoryVisible: boolean;
  { Note: while we try to always sensibly update InventoryCurrentItem,
    to keep the assumptions that
    1. Player.Items.Count = 0 => InventoryCurrentItem = -1
    2. Player.Items.Count > 0 =>
       InventoryCurrentItem between 0 and Player.Items.Count - 1

    but you should *nowhere* depend on these assuptions.
    That's because I want to allow myself freedom to modify Items
    in various situations, so InventoryCurrentItem can become
    invalid in many situations.

    So every code should check that
    - If InventoryCurrentItem between 0 and Player.Items.Count - 1
      that InventoryCurrentItem is selected
    - Else no item is selected (possibly Player.Items.Count = 0,
      possibly not) }
  InventoryCurrentItem: Integer;

  ShowDebugInfo: boolean;

  DisplayFpsUpdateTick: TMilisecTime;
  DisplayFpsFrameTime: Single;
  DisplayFpsRealTime: Single;

const
  SDeadMessage = 'You''re dead. Press [Escape] to exit to menu';

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

  procedure DoDrawInventory;
  const
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
    I, X, Y, MaxItemShown: Integer;
    S: string;
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
      Font_BFNT_BitstreamVeraSans_m10.Print(S);
    end;
  end;

  procedure DoShowDebugInfo;
  begin
    glColorv(Vector3Single(0.7, 0.7, 0.7));
    glRasterPos2i(0, RequiredScreenHeight -
      Font_BFNT_BitstreamVeraSans.RowHeight * 2 - 10 { margin });

    { Don't display precise Glw.FpsFrameTime and Glw.FpsRealTime
      each time --- this would cause too much move for player.
      Instead, display DisplayFpsXxxTime that are updated each second. }
    if (DisplayFpsUpdateTick = 0) or
       (TimeTickDiff(DisplayFpsUpdateTick, GetTickCount) >= 1000) then
    begin
      DisplayFpsUpdateTick := GetTickCount;
      DisplayFpsFrameTime := Glw.FpsFrameTime;
      DisplayFpsRealTime := Glw.FpsRealTime;
    end;

    Font_BFNT_BitstreamVeraSans.Print(
      Format('FPS : %f (real : %f)', [DisplayFpsFrameTime, DisplayFpsRealTime]));
  end;

  procedure DoShowDeadInfo;
  begin
    glColorv(Vector3Single(1, 0, 0));
    glRasterPos2i(0, RequiredScreenHeight -
      Font_BFNT_BitstreamVeraSans.RowHeight * 3 - 15 { margin });
    Font_BFNT_BitstreamVeraSans.Print(SDeadMessage);
  end;

begin
  Player.RenderWeapon2D;

  glCallList(GLList_Draw2dBegin);

  GameMessagesManager.Draw2d(RequiredScreenWidth, RequiredScreenHeight,
    Glw.Width, Glw.Height);

  if InventoryVisible then
    DoDrawInventory;

  if ShowDebugInfo then
    DoShowDebugInfo;

  if Player.Dead then
    DoShowDeadInfo;

  Player.Render2D;
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

  { Rendering order of Creatures, Items and Level:
    You know the problem. We must first render all non-transparent objects,
    then all transparent objects. Otherwise transparent objects
    (that must be rendered without updating depth buffer) could get brutally
    covered by non-transparent objects (that are in fact further away from
    the camera). So we first render all non-transparent creatures and items,
    then the level, then all transparent creatures and items.

    TODO: actually, right now I workaround this by first rendering
    creatures (there is no transparent creature), then level,
    then items (there is a transparent item --- life potion). }

  Level.Creatures.Render(Player.Navigator.Frustum);
  Level.Render(Player.Navigator.Frustum);
  Level.Items.Render(Player.Navigator.Frustum);

  Player.RenderAttack;

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    ProjectionGLPushPop(Draw2d, 0, Ortho2dProjMatrix(
      0, RequiredScreenWidth, 0, RequiredScreenHeight));
  glPopAttrib;
end;

procedure Idle(Glwin: TGLWindow);
var
  PickItemIndex, PlayerItemIndex: Integer;
begin
  GameMessagesManager.Idle;
  Level.Idle(Glw.FpsCompSpeed);
  Level.Items.Idle(Glw.FpsCompSpeed);
  Level.Creatures.Idle(Glw.FpsCompSpeed);
  Level.Creatures.RemoveFromLevel;

  if not Player.Dead then
  begin
    PickItemIndex := Level.Items.PlayerCollision;
    if PickItemIndex <> -1 then
    begin
      PlayerItemIndex := Player.PickItem(Level.Items[PickItemIndex].ExtractItem);
      Level.Items.FreeAndNil(PickItemIndex);
      Level.Items.Delete(PickItemIndex);

      { update InventoryCurrentItem. }
      if not Between(InventoryCurrentItem, 0, Player.Items.Count - 1) then
        InventoryCurrentItem := PlayerItemIndex;
    end;
  end;

  if (not Level.HintButtonShown) and
     Box3dPointInside(Player.Navigator.CameraPos, Level.HintButtonBox) then
  begin
    GameMessage('Hint: you can press this red button by clicking with mouse on it');
    Level.HintButtonShown := true;
  end;

  Player.Idle(Glw.FpsCompSpeed);
end;

procedure Timer(Glwin: TGLWindow);
begin
  if ALActive then CheckAL('game loop (check in OnTimer)');
end;

procedure GameCancel(RequireConfirmation: boolean);
begin
  if Player.Dead or
    (not RequireConfirmation) or
    MessageYesNo(Glw, 'Are you sure you want to end the game ?', taLeft) then
    GameEnded := true;
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

  procedure UpdateInventoryCurrentItemAfterDelete;
  begin
    { update InventoryCurrentItem.
      Note that if Player.Items.Count = 0 now, then this will
      correctly set InventoryCurrentItem to -1. }
    if InventoryCurrentItem >= Player.Items.Count then
      InventoryCurrentItem := Player.Items.Count - 1;
  end;

  procedure DropItem;
  var
    DropppedItem: TItem;
    DropPosition, PushVector: TVector3Single;
    IsAboveTheGround: boolean;
    SqrHeightAboveTheGround: Single;
    PushVectorLength: Single;
  begin
    if Player.Dead then
    begin
      GameMessage(SDeadMessage);
      Exit;
    end;

    if Between(InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      DropppedItem := Player.DropItem(InventoryCurrentItem);
      if DropppedItem <> nil then
      begin
        UpdateInventoryCurrentItemAfterDelete;

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
    if Player.Dead then
    begin
      GameMessage(SDeadMessage);
      Exit;
    end;

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

      UpdateInventoryCurrentItemAfterDelete;
    end else
      GameMessage('Nothing to use - select some item first');
  end;

  procedure CancelFlying;
  begin
    if not Player.Dead then
      Player.CancelFlying else
      GameMessage(SDeadMessage);
  end;

  procedure MaybeDeadMessage;
  begin
    if Player.Dead then
      GameMessage(SDeadMessage);
  end;

  procedure DoAttack;
  begin
    if not Player.Dead then
      Player.Attack else
      GameMessage(SDeadMessage);
  end;

begin
  { Basic keys. }
  if Key = CastleKey_Attack.Value then
    DoAttack else
  if (Key = CastleKey_UpMove.Value) or
     (Key = CastleKey_DownMove.Value) or
     (Key = CastleKey_Forward.Value) or
     (Key = CastleKey_Backward.Value) or
     (Key = CastleKey_LeftRot.Value) or
     (Key = CastleKey_RightRot.Value) or
     (Key = CastleKey_LeftStrafe.Value) or
     (Key = CastleKey_RightStrafe.Value) or
     (Key = CastleKey_UpRotate.Value) or
     (Key = CastleKey_DownRotate.Value) or
     (Key = CastleKey_HomeUp.Value) then
    MaybeDeadMessage else

  { Items keys. }
  if Key = CastleKey_InventoryShow.Value then
    InventoryVisible := not InventoryVisible else
  if Key = CastleKey_InventoryPrevious.Value then
    ChangeInventoryCurrentItem(-1) else
  if Key = CastleKey_InventoryNext.Value then
    ChangeInventoryCurrentItem(+1) else
  if Key = CastleKey_DropItem.Value then
    DropItem else
  if Key = CastleKey_UseItem.Value then
    UseItem else

  { Other keys. }
  if Key = CastleKey_SaveScreen.Value then
    SaveScreen else
  if Key = CastleKey_ViewMessages.Value then
    ViewGameMessages else
  if Key = CastleKey_CancelFlying.Value then
    CancelFlying else
  if Key = CastleKey_FPSShow.Value then
    ShowDebugInfo := not ShowDebugInfo else

  { Fixed keys. }
  if C = CharEscape then
    ShowGameMenu;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;

procedure MouseDown(Glwin: TGLWindow; Btn: TMouseButton);
type
  { TODO: This will be expanded with at least poEnemy in the future. }
  TPickedObjectType = (poNone, poLevel, poItem, poSpecialObject);
var
  Ray0, RayVector: TVector3Single;
  LevelCollisionIndex, ItemCollisionIndex, SpecialObjectIndex, I: integer;
  IntersectionDistance, ThisIntersectionDistance: Single;
  PickedObjectType: TPickedObjectType;
begin
  if Btn = mbLeft then
  begin
    if Player.Dead then
    begin
      GameMessage(SDeadMessage);
      Exit;
    end;

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

    { Collision with Level.SpecialObjects }
    SpecialObjectIndex := Level.SpecialObjectsTryPick(
      ThisIntersectionDistance, Ray0, RayVector);
    if (SpecialObjectIndex <> -1) and
       ( (PickedObjectType = poNone) or
         (ThisIntersectionDistance < IntersectionDistance)
       ) then
    begin
      PickedObjectType := poSpecialObject;
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
      poSpecialObject:
        begin
          Level.SpecialObjectPicked(
            IntersectionDistance, SpecialObjectIndex);
        end;
    end;
  end;
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
begin
  Level := ALevel;
  Player := APlayer;
  InventoryVisible := false;
  InventoryCurrentItem := -1;
  try

    SavedMode := TGLMode.Create(glw,
      { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
      GL_ENABLE_BIT, true);
    try
      { init navigator }
      Glw.Navigator := Player.Navigator;
      try
        { Init Player.Navigator properties }
        Player.Navigator.OnMatrixChanged := TDummy.MatrixChanged;
        Player.Navigator.OnMoveAllowed := Level.PlayerMoveAllowed;
        Player.Navigator.OnGetCameraHeight := Level.PlayerGetCameraHeight;

        { Init initial camera pos }
        Player.Navigator.Init(Level.HomeCameraPos, Level.HomeCameraDir,
          Level.HomeCameraUp, Level.CameraPreferredHeight,
          0.0 { Level.CameraPreferredHeight is already corrected if necessary,
                so I pass here 0.0 instead of CameraRadius } );

        { tests:
          InfoWrite(Format('%f %f %f %f',
            [VectorLen(Player.Navigator.HomeCameraDir),
            VectorLen(Player.Navigator.CameraDir),
            Player.Navigator.CameraPreferredHeight,
            Player.Navigator.MoveSpeed])); }

        { Note that this sets AutoRedisplay to true. }
        SetStandardGLWindowState(Glw, Draw, CloseQuery, Resize,
          nil, { AutoRedisplay } true, { FPSActive } true, { MenuActive } false,
          K_None, #0, { FpsShowOnCaption } false, { UseNavigator } true);

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

        GLWinMessagesTheme.RectColor[3] := 0.4;

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

procedure SaveScreen;
var
  FileName: string;
begin
  FileName := FnameAutoInc(ApplicationName + '_screen_%d.png');
  Glw.SaveScreen(FileName);
  GameMessage('Screen saved to ' + FileName);
  Sound(stSaveScreen);
end;

{ initialization / finalization ---------------------------------------------- }

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

  Font_BFNT_BitstreamVeraSans_m10 := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans_m10);
  Font_BFNT_BitstreamVeraSans     := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  FreeAndNil(Font_BFNT_BitstreamVeraSans);
  FreeAndNil(Font_BFNT_BitstreamVeraSans_m10);

  glFreeDisplayList(GLList_Draw2dBegin);
  glFreeDisplayList(GLList_InventorySlot);
end;

initialization
  GameMessages := TStringList.Create;
  ShowDebugInfo := false;
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);
finalization
  FreeAndNil(GameMessages);
end.