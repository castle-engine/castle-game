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

{ Play the game.

  This initializes Level and Player global variables to
  ALevel and APlayer. Upon exit, it will set Player and Level
  back to nil.

  Note that Level may change during the PlayLevel, because
  of LevelFinished. In such case old Level value will be freeed,
  and Level will be set to new value. Last Level value should
  be freed be the called of PlayLevel. ALevel upon exit is set to this
  last Level value (global Level value is then set to nil, so it
  becomes useless). }
procedure PlayLevel(var ALevel: TLevel; APlayer: TPlayer);

var
  { Currently used player by PlayLevel. nil if PlayLevel doesn't work
    right now.
    @noAutoLinkHere }
  Player: TPlayer;

  { Currently used level by PlayLevel. nil if PlayLevel doesn't work
    right now.
    @noAutoLinkHere }
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

{ If NextLevel = nil, then end the game.
  Else free current Level and set Level to NextLevel.

  Note that this doesn't work immediately, but will perform
  at nearest possibility. While LevelFinished is scheduled but not
  performed yet, you of course can't call LevelFinished once again
  with different NextLevel. }
procedure LevelFinished(NextLevel: TLevel);

{ Saves a screen, causing also appropriate GameMessage. }
procedure SaveScreen;

{ ViewAngleDegX and ViewAngleDegY specify field of view in the game.
  You can freely change ViewAngleDegX at runtime, just make sure
  that our OnResize will be called after. }
var
  ViewAngleDegX: Single = 70.0;
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
  KambiFilesUtils, CastleKeys, CastleGameMenu, CastleSound,
  CastleVideoOptions;

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

  LevelFinishedSchedule: boolean = false;
  { If LevelFinishedSchedule, then this is non-nil, and should be the next
    value of Level. }
  LevelFinishedNextLevel: TLevel;

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

  procedure RenderFrontShadowQuads;
  var
    I: Integer;
  begin
    for I := 0 to Level.Creatures.High do
    begin
      Level.Creatures.Items[I].RenderFrontShadowQuads(
        Level.LightCastingShadowsPosition,
        Player.Navigator.CameraPos);
    end;
  end;

  procedure RenderBackShadowQuads;
  var
    I: Integer;
  begin
    for I := 0 to Level.Creatures.High do
      Level.Creatures.Items[I].RenderBackShadowQuads;
  end;

const
  { Which stencil bits should be tested when determining which things
    are in the scene ?

    Not only *while rendering* shadow quads but also *after this rendering*
    value in stencil buffer may be > 1 (so you need more than 1 bit
    to hold it in stencil buffer).

    Why ? It's obvious that *while rendering* (e.g. right after rendering
    all front quads) this value may be > 1. But when the point
    is in the shadow because it's inside more than one shadow
    (cast by 2 different shadow quads) then even *after rendering*
    this point will have value > 1.

    So it's important that this constant spans a couple of bits.
    More precisely, it should be the maximum number of possibly overlapping
    front shadow quads from any possible camera view. }
  StencilShadowBits = $FF;
var
  ClearBuffers: TGLbitfield;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if RenderShadowsPossible and RenderShadows then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  if Level.Scene.Background <> nil then
  begin
    glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
    Level.Scene.Background.Render;
  end else
    ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

  { Now clear buffers indicated in ClearBuffers. }
  glClear(ClearBuffers);

  glLoadMatrix(Glw.Navigator.Matrix);

  Level.LightSet.RenderLights;

  if RenderShadowsPossible and RenderShadows then
  begin
    glPushAttrib(GL_LIGHTING_BIT);
      { Headlight will stay on here, but lights in Level.LightSet are off.
        More precise would be to turn off only the light that
        is casting shadows (Level.LightCastingShadowsPosition),
        but, well, this is only an approximation :) }
      Level.LightSet.TurnLightsOff;
      Level.Render(Player.Navigator.Frustum);
    glPopAttrib;

    glEnable(GL_STENCIL_TEST);
      { Note that stencil buffer is set to all 0 now. }

      glPushAttrib(GL_ENABLE_BIT);
        glEnable(GL_DEPTH_TEST);
        { Calculate shadows to the stencil buffer.
          Don't write anything to depth or color buffers. }
        glSetDepthAndColorWriteable(GL_FALSE);
          { For each fragment that passes depth-test, *increase* it's stencil
            value by 1. Render front facing shadow quads. }
          glStencilFunc(GL_ALWAYS, 0, 0);
          glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
          RenderFrontShadowQuads;
          { For each fragment that passes depth-test, *decrease* it's stencil
            value by 1. Render back facing shadow quads. }
          glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
          RenderBackShadowQuads;
        glSetDepthAndColorWriteable(GL_TRUE);
      glPopAttrib;
    glDisable(GL_STENCIL_TEST);

    { Now render everything once again, with lights turned on.
      But render only things not in shadow. }
    glClear(GL_DEPTH_BUFFER_BIT);

    { Creatures and items are never in shadow (this looks bad).
      So I just render them here, when the lights are turned on
      and ignoring stencil buffer. I have to do this *after*
      glClear(GL_DEPTH_BUFFER_BIT) above. }
    Level.Creatures.Render(Player.Navigator.Frustum, false);
    Level.Items.Render(Player.Navigator.Frustum, false);

    { setup stencil : don't modify stencil, stencil test passes only for =0 }
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glStencilFunc(GL_EQUAL, 0, StencilShadowBits);
    glEnable(GL_STENCIL_TEST);
      Level.Render(Player.Navigator.Frustum);
    glDisable(GL_STENCIL_TEST);

    if CastleVideoOptions.RenderShadowQuads then
    begin
      glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
        glEnable(GL_DEPTH_TEST);
        glDisable(GL_LIGHTING);
        glColor4f(1, 1, 0, 0.3);
        glDepthMask(GL_FALSE);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
        RenderFrontShadowQuads;
      glPopAttrib;
    end;
  end else
  begin
    Level.Creatures.Render(Player.Navigator.Frustum, false);
    Level.Items.Render(Player.Navigator.Frustum, false);
    Level.Render(Player.Navigator.Frustum);
  end;

  { Rendering order of Creatures, Items and Level:
    You know the problem. We must first render all non-transparent objects,
    then all transparent objects. Otherwise transparent objects
    (that must be rendered without updating depth buffer) could get brutally
    covered by non-transparent objects (that are in fact further away from
    the camera).

    For simplicity, I decided that for now creatures and items are not allowed
    to be partially transparent and partially opaque.
    So we first render all non-transparent creatures and items,
    then the level, then all transparent creatures and items. }
  Level.Creatures.Render(Player.Navigator.Frustum, true);
  Level.Items.Render(Player.Navigator.Frustum, true);

  Player.RenderAttack;

  glPushAttrib(GL_ENABLE_BIT);
    glDisable(GL_LIGHTING);
    ProjectionGLPushPop(Draw2d, 0, Ortho2dProjMatrix(
      0, RequiredScreenWidth, 0, RequiredScreenHeight));
  glPopAttrib;
end;

{ Call this when Level value changed (because of LevelFinished
  or because we just started new game). }
procedure InitNewLevel;
const
  HeadlightPower = 0.5;
begin
  { Resize uses Level.BoundingBox to set good projection min/max depths.
    So we must call EventResize on each Level change. }
  Glw.EventResize;

  { Init Player.Navigator properties }
  Player.Navigator.OnMoveAllowed := Level.PlayerMoveAllowed;
  Player.Navigator.OnGetCameraHeight := Level.PlayerGetCameraHeight;

  { Init initial camera pos }
  Player.Navigator.Init(Level.HomeCameraPos, Level.HomeCameraDir,
    Level.HomeCameraUp, Level.CameraPreferredHeight,
    0.0 { Level.CameraPreferredHeight is already corrected if necessary,
          so I pass here 0.0 instead of CameraRadius } );

  Player.Navigator.CancelFallingDown;

  { Init headlight }
  if Level.Headlight then
  begin
    glEnable(GL_LIGHT0);
    glLightv(GL_LIGHT0, GL_DIFFUSE,
      Vector4Single(HeadlightPower, HeadlightPower, HeadlightPower, 1));
    glLightv(GL_LIGHT0, GL_SPECULAR,
      Vector4Single(HeadlightPower, HeadlightPower, HeadlightPower, 1));
  end else
    glDisable(GL_LIGHT0);

  { First GameMessage for this level. }
  GameMessage('Loaded level "' + Level.Title + '"');
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

  if LevelFinishedSchedule then
  begin
    LevelFinishedSchedule := false;
    FreeAndNil(Level);
    Level := LevelFinishedNextLevel;
    InitNewLevel;
  end;
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

    function GetItemDropPosition(
      DroppedItemKind: TItemKind;
      var DropPosition: TVector3Single): boolean;
    var
      ItemBox: TBox3d;
      ItemBoxRadius: Single;
      ItemBoxMiddle: TVector3Single;
      PushVector: TVector3Single;
      PushVectorLength: Single;
    begin
      ItemBox := DroppedItemKind.BoundingBoxRotated;
      ItemBoxMiddle := Box3dMiddle(ItemBox);
      { Box3dRadius calculates radius around (0, 0, 0) and we want
        radius around ItemBoxMiddle }
      ItemBoxRadius := Box3dRadius(
        Box3dTranslate(ItemBox, VectorNegate(ItemBoxMiddle)));

      { Calculate DropPosition.

        We must move the item a little before us to
        1. show visually player that the item was dropped
        2. to avoid automatically picking it again

        Note that I take PushVector from CameraDirInHomePlane,
        not from CameraDir, otherwise when player is looking
        down he could be able to put item "inside the ground".
        Collision detection with the level below would actually
        prevent putting item "inside the ground", but the item
        would be too close to the player --- he could pick it up
        immediately. }
      PushVector := Player.Navigator.CameraDirInHomePlane;
      PushVectorLength := Max(
        Player.Navigator.RealCameraPreferredHeight,
        Box3dSizeX(ItemBox) * 2,
        Box3dSizeY(ItemBox) * 2);
      VectorAdjustToLengthTo1st(PushVector, PushVectorLength);
      DropPosition := VectorAdd(Player.Navigator.CameraPos,
        PushVector);

      { Now check is DropPosition actually possible
        (i.e. check collisions item<->level).
        The assumption is that item starts from
        Player.Navigator.CameraPos and is moved to DropPosition.

        But actually we must shift both these positions,
        so that we check positions that are ideally in the middle
        of item's BoundingBoxRotated. Otherwise the item
        could get *partially* stuck within the wall, which wouldn't
        look good. }

      Result := Level.MoveAllowedSimple(
        VectorAdd(Player.Navigator.CameraPos, ItemBoxMiddle),
        VectorAdd(DropPosition, ItemBoxMiddle),
        false, ItemBoxRadius)
    end;

  var
    DropppedItem: TItem;
    DropPosition: TVector3Single;
  begin
    if Player.Dead then
    begin
      GameMessage(SDeadMessage);
      Exit;
    end;

    if Between(InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      if GetItemDropPosition(Player.Items[InventoryCurrentItem].Kind,
        DropPosition) then
      begin
        DropppedItem := Player.DropItem(InventoryCurrentItem);
        if DropppedItem <> nil then
        begin
          UpdateInventoryCurrentItemAfterDelete;
          Level.Items.Add(TItemOnLevel.Create(DropppedItem, DropPosition));
        end;
      end else
        GameMessage('Not enough room here to drop this item');
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

  procedure DoGameMenu;
  var
    ViewAngleChanged: boolean;
  begin
    ShowGameMenu(ViewAngleChanged);
    if ViewAngleChanged then
      Glwin.EventResize;
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
    DoGameMenu;
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
      ThisIntersectionDistance, Ray0, RayVector, true, NoItemIndex, false, nil);
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

procedure PlayLevel(var ALevel: TLevel; APlayer: TPlayer);
var
  SavedMode: TGLMode;
begin
  Level := ALevel;
  Player := APlayer;
  InventoryVisible := false;
  InventoryCurrentItem := -1;
  LevelFinishedSchedule := false;
  try

    SavedMode := TGLMode.Create(glw,
      { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
      GL_ENABLE_BIT, true);
    try
      SavedMode.FakeMouseDown := false;

      { init navigator }
      Glw.Navigator := Player.Navigator;
      try
        { Init Player.Navigator properties }
        Player.Navigator.OnMatrixChanged := TDummy.MatrixChanged;

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

        GameMessagesManager := TTimeMessagesManager.Create(
          Glw, hpMiddle, vpDown, Glw.Width);
        try
          GameMessagesManager.MaxMessagesCount := 4;

          InitNewLevel;

          GameEnded := false;

          glEnable(GL_LIGHTING);

          GLWinMessagesTheme.RectColor[3] := 0.4;

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
    ALevel := Level;
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
    if LevelFinishedSchedule and (LevelFinishedNextLevel <> NextLevel) then
      raise EInternalError.Create(
        'You cannot call LevelFinished while previous LevelFinished is not done yet');

    LevelFinishedSchedule := true;
    LevelFinishedNextLevel := NextLevel;
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