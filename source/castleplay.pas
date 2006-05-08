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

  Note that Level may change during the PlayGame, because
  of LevelFinished. In such case old Level value will be freeed,
  and Level will be set to new value. Last Level value should
  be freed by the caller of PlayGame. ALevel upon exit is set to this
  last Level value (global Level value is then set to nil, so it
  becomes useless).

  If PrepareNewPlayer then it will call Level.PrepareNewPlayer
  right before starting the actual game. }
procedure PlayGame(var ALevel: TLevel; APlayer: TPlayer;
  PrepareNewPlayer: boolean);

var
  { Currently used player by PlayGame. nil if PlayGame doesn't work
    right now.
    @noAutoLinkHere }
  Player: TPlayer;

  { Currently used level by PlayGame. nil if PlayGame doesn't work
    right now.
    @noAutoLinkHere }
  Level: TLevel;

{ If NextLevel = nil, then end the game.
  Else free current Level and set Level to NextLevel.

  Note that this doesn't work immediately, but will perform
  at nearest possibility. While LevelFinished is scheduled but not
  performed yet, you of course can't call LevelFinished once again
  with different NextLevel. }
procedure LevelFinished(NextLevel: TLevel);

{ Saves a screen, causing also appropriate TimeMessage. }
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

  { Read-only from outside of this unit. Initially false when starting
    PlayGame. }
  GameWin: boolean;

{ Note that when Player.Dead or GameWin,
  confirmation will never be required anyway. }
procedure GameCancel(RequireConfirmation: boolean);

const
  DefaultAutoOpenInventory = true;

var
  { Automatically open inventory on pickup ?
    Saved/loaded to config file in this unit. }
  AutoOpenInventory: boolean;

implementation

uses Math, SysUtils, KambiUtils, GLWindow, VRMLRayTracer, OpenAL, ALUtils,
  GLWinModes, OpenGLh, KambiGLUtils, GLWinMessages, CastleWindow,
  MatrixNavigation, VectorMath, Boxes3d, Images,
  CastleHelp, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit,
  BFNT_BitstreamVeraSans_Unit, CastleCreatures,
  CastleItems, VRMLTriangleOctree, RaysWindow, KambiStringUtils,
  KambiFilesUtils, CastleKeys, CastleGameMenu, CastleSound,
  CastleVideoOptions, Keys, CastleConfig, GLHeadlight, CastleThunder,
  CastleTimeMessages, BackgroundGL;

var
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
  SGameWinMessage = 'Game finished. Press [Escape] to exit to menu';

function ViewAngleDegY: Single;
begin
  Result := AdjustViewAngleDegToAspectRatio(ViewAngleDegX,
    Glw.Height / Glw.Width);
end;

{ If ALActive then update listener POSITION and ORIENTATION
  based on Player.Navigator.Camera* }
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

  procedure DoShowGameWinInfo;
  begin
    glColorv(Vector3Single(0.8, 0.8, 0.8));
    glRasterPos2i(0, RequiredScreenHeight -
      Font_BFNT_BitstreamVeraSans.RowHeight * 3 - 15 { margin });
    Font_BFNT_BitstreamVeraSans.Print(SGameWinMessage);
  end;

begin
  Player.RenderWeapon2D;

  glCallList(GLList_Draw2dBegin);

  TimeMessagesDraw;

  if InventoryVisible then
    DoDrawInventory;

  if ShowDebugInfo then
    DoShowDebugInfo;

  if Player.Dead then
    DoShowDeadInfo;

  if GameWin then
    DoShowGameWinInfo;

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

  procedure RenderCreaturesItems(const Transparent: boolean);
  begin
    { When GameWin, don't render creatures (as we don't check
      collisions when MovingPlayerEndSequence). }
    if not GameWin then
      Level.Creatures.Render(Player.Navigator.Frustum, Transparent);
    Level.Items.Render(Player.Navigator.Frustum, Transparent);
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
  UsedBackground: TBackgroundGL;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if RenderShadowsPossible and RenderShadows then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  if (Level is TCagesLevel) and TCagesLevel(Level).DoEndSequence then
    UsedBackground := TCagesLevel(Level).EndSequence.Background else
    UsedBackground := Level.Scene.Background;

  if UsedBackground <> nil then
  begin
    glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
    UsedBackground.Render;
  end else
    ClearBuffers := ClearBuffers or GL_COLOR_BUFFER_BIT;

  { Now clear buffers indicated in ClearBuffers. }
  glClear(ClearBuffers);

  glLoadMatrix(Glw.Navigator.Matrix);

  TThunderEffect.RenderOrDisable(Level.ThunderEffect, 1);
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
    RenderCreaturesItems(false);

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
    RenderCreaturesItems(false);
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
  RenderCreaturesItems(true);

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

  { Init Level.Headlight }
  TGLHeadlight.RenderOrDisable(Level.Headlight, 0);

  if Level.ThunderEffect <> nil then
    Level.ThunderEffect.InitGLLight(1);

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Level.GlobalAmbientLight);

  MusicPlayer.PlayedSound := Level.PlayedMusicSound;

  { First TimeMessage for this level. }
  TimeMessage('Loaded level "' + Level.Title + '"');
end;

procedure Idle(Glwin: TGLWindow);
var
  CompSpeed: Single;

  procedure ModifyPlayerEndSequence(
    const TargetPosition: TVector3Single;
    const TargetDirection: TVector3Single;
    const TargetUp: TVector3Single);
  const
    PositionChangeSpeed = 0.05;
    DirectionChangeSpeed = 0.01;
    UpChangeSpeed = 0.01;
  var
    ToPosition: TVector3Single;
    ToDirection: TVector3Single;
    ToUp: TVector3Single;

    ToPositionLength: Single;
    ToDirectionLength: Single;
    ToUpLength: Single;
  begin
    ToPosition  := VectorSubtract(TargetPosition , Player.Navigator.CameraPos);
    ToDirection := VectorSubtract(TargetDirection, Player.Navigator.CameraDir);
    ToUp        := VectorSubtract(TargetUp       , Player.Navigator.CameraUp);

    ToPositionLength  := VectorLen(ToPosition);
    ToDirectionLength := VectorLen(ToDirection);
    ToUpLength        := VectorLen(ToUp);

    if IsZero(ToPositionLength) and
       IsZero(ToDirectionLength) and
       IsZero(ToUpLength) then
      TCagesLevel(Level).DoEndSequence := true else
    begin
      if ToPositionLength < CompSpeed * PositionChangeSpeed then
        Player.Navigator.CameraPos := TargetPosition else
        Player.Navigator.CameraPos := VectorAdd(
          Player.Navigator.CameraPos,
          VectorAdjustToLength(ToPosition, CompSpeed * PositionChangeSpeed));

      if ToDirectionLength < CompSpeed * DirectionChangeSpeed then
        Player.Navigator.CameraDir := TargetDirection else
        Player.Navigator.CameraDir := VectorAdd(
          Player.Navigator.CameraDir,
          VectorAdjustToLength(ToDirection, CompSpeed * DirectionChangeSpeed));

      if ToUpLength < CompSpeed * UpChangeSpeed then
        Player.Navigator.CameraUp := TargetUp else
        Player.Navigator.CameraUp := VectorAdd(
          Player.Navigator.CameraUp,
          VectorAdjustToLength(ToUp, CompSpeed * UpChangeSpeed));
    end;
  end;

const
  GameWinPosition1: TVector3Single = (30.11, 146.27, 1.80);
  GameWinPosition2: TVector3Single = (30.11, 166.27, 1.80);
  GameWinDirection: TVector3Single = (0, 1, 0);
  GameWinUp: TVector3Single = (0, 0, 1);
var
  PickItemIndex, PlayerItemIndex: Integer;
begin
  CompSpeed := Glw.IdleCompSpeed;

  TimeMessagesIdle;

  Level.Idle(CompSpeed);
  Level.Items.Idle(CompSpeed);

  if not GameWin then
    Level.Creatures.Idle(CompSpeed);
  Level.Creatures.RemoveFromLevel;

  if (not Player.Dead) and (not GameWin) then
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

      if AutoOpenInventory then
        InventoryVisible := true;
    end;
  end;

  if (not Level.HintButtonShown) and
     Box3dPointInside(Player.Navigator.CameraPos, Level.HintButtonBox) then
  begin
    TimeMessage('Hint: press this red button by clicking on it with ' +
      'right mouse button');
    Level.HintButtonShown := true;
  end;

  Player.Idle(CompSpeed);

  if LevelFinishedSchedule then
  begin
    LevelFinishedSchedule := false;
    FreeAndNil(Level);
    Level := LevelFinishedNextLevel;
    InitNewLevel;
  end;

  if GameWin and (Level is TCagesLevel) then
  begin
    if TCagesLevel(Level).DoEndSequence then
      ModifyPlayerEndSequence(GameWinPosition2, GameWinDirection, GameWinUp) else
      ModifyPlayerEndSequence(GameWinPosition1, GameWinDirection, GameWinUp);
  end;
end;

procedure Timer(Glwin: TGLWindow);
begin
  if ALActive then
  begin
    CheckAL('game loop (check in OnTimer)');
    ALRefreshUsedSources;
  end;
end;

procedure GameCancel(RequireConfirmation: boolean);
begin
  if Player.Dead or GameWin or (not RequireConfirmation) or
    MessageYesNo(Glw, 'Are you sure you want to end the game ?', taLeft) then
    GameEnded := true;
end;

procedure DoAttack;
begin
  if GameWin then
    TimeMessage(SGameWinMessage) else
  if not Player.Dead then
    Player.Attack else
    TimeMessage(SDeadMessage);
end;

procedure DoInteract;

  function TryInteract(RayVector: TVector3Single): boolean;
  type
    { TODO: This will be expanded with at least poEnemy in the future. }
    TPickedObjectType = (poNone, poLevel, poItem, poSpecialObject);
  var
    Ray0: TVector3Single;
    LevelCollisionIndex, ItemCollisionIndex, SpecialObjectIndex, I: integer;
    IntersectionDistance, ThisIntersectionDistance: Single;
    PickedObjectType: TPickedObjectType;
  begin
    Ray0 := Player.Navigator.CameraPos;

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
          Result := Level.TrianglePicked(IntersectionDistance,
            Level.Scene.DefaultTriangleOctree.OctreeItems.
              Items[LevelCollisionIndex]);
        end;
      poItem:
        begin
          Level.Items[ItemCollisionIndex].ItemPicked(IntersectionDistance);
          Result := true;
        end;
      poSpecialObject:
        begin
          Level.SpecialObjectPicked(
            IntersectionDistance, SpecialObjectIndex);
          Result := true;
        end;
      else
        Result := false;
    end;
  end;

  function TryInteractAround(const XChange, YChange: Integer): boolean;
  var
    RayVector: TVector3Single;
  begin
    RayVector := PrimaryRay(
      MiddleScreenWidth + XChange, MiddleScreenHeight + YChange,
      Glw.Width, Glw.Height,
      Player.Navigator.CameraPos, Player.Navigator.CameraDir,
      Player.Navigator.CameraUp,
      ViewAngleDegX, ViewAngleDegY);
    Result := TryInteract(RayVector);
  end;

  function TryInteractAroundSquare(const Change: Integer): boolean;
  begin
    Result := TryInteractAround(-Change, -Change) or
              TryInteractAround(-Change, +Change) or
              TryInteractAround(+Change, +Change) or
              TryInteractAround(+Change, -Change) or
              TryInteractAround(      0, -Change) or
              TryInteractAround(      0, +Change) or
              TryInteractAround(-Change,       0) or
              TryInteractAround(+Change,       0);
  end;

begin
  if GameWin then
  begin
    TimeMessage(SGameWinMessage);
    Exit;
  end;

  if Player.Dead then
  begin
    TimeMessage(SDeadMessage);
    Exit;
  end;

  { Try to interact with the object in the middle --- if nothing interesting
    there, try to interact with things around the middle. }
  if not TryInteract(Player.Navigator.CameraDir) then
    if not TryInteractAroundSquare(25) then
      if not TryInteractAroundSquare(50) then
        if not TryInteractAroundSquare(100) then
          if not TryInteractAroundSquare(200) then
            Sound(stPlayerInteractFailed);
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
    if GameWin then
    begin
      TimeMessage(SGameWinMessage);
      Exit;
    end;

    if Player.Dead then
    begin
      TimeMessage(SDeadMessage);
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
        TimeMessage('Not enough room here to drop this item');
    end else
      TimeMessage('Nothing to drop - select some item first');
  end;

  procedure UseItem;
  var
    UsedItem: TItem;
    UsedItemIndex: Integer;
  begin
    if GameWin then
    begin
      TimeMessage(SGameWinMessage);
      Exit;
    end;

    if Player.Dead then
    begin
      TimeMessage(SDeadMessage);
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
      TimeMessage('Nothing to use - select some item first');
  end;

  procedure CancelFlying;
  begin
    if GameWin then
      TimeMessage(SGameWinMessage) else
    if not Player.Dead then
      Player.CancelFlying else
      TimeMessage(SDeadMessage);
  end;

  procedure MaybeDeadWinMessage;
  begin
    if GameWin then
      TimeMessage(SGameWinMessage) else
    if Player.Dead then
      TimeMessage(SDeadMessage);
  end;

  procedure MaybeWinMessage;
  begin
    if GameWin then
      TimeMessage(SGameWinMessage);
  end;

  procedure DoGameMenu;
  begin
    ShowGameMenu(Draw);
  end;

begin
  { Basic keys. }
  if CastleKey_Attack.IsValue(Key) then
    DoAttack else
  if CastleKey_UpMove.IsValue(Key) or
     CastleKey_DownMove.IsValue(Key) or
     CastleKey_Forward.IsValue(Key) or
     CastleKey_Backward.IsValue(Key) or
     CastleKey_LeftStrafe.IsValue(Key) or
     CastleKey_RightStrafe.IsValue(Key) then
    MaybeDeadWinMessage else
  if { Note that rotation keys work even when player is dead.
       See comments in TPlayer.UpdateNavigator. }
     CastleKey_LeftRot.IsValue(Key) or
     CastleKey_RightRot.IsValue(Key) or
     CastleKey_UpRotate.IsValue(Key) or
     CastleKey_DownRotate.IsValue(Key) or
     CastleKey_HomeUp.IsValue(Key) then
    MaybeWinMessage else

  { Items keys. }
  if CastleKey_InventoryShow.IsValue(Key) then
    InventoryVisible := not InventoryVisible else
  if CastleKey_InventoryPrevious.IsValue(Key) then
    ChangeInventoryCurrentItem(-1) else
  if CastleKey_InventoryNext.IsValue(Key) then
    ChangeInventoryCurrentItem(+1) else
  if CastleKey_DropItem.IsValue(Key) then
    DropItem else
  if CastleKey_UseItem.IsValue(Key) then
    UseItem else

  { Other keys. }
  if CastleKey_SaveScreen.IsValue(Key) then
    SaveScreen else
  if CastleKey_ViewMessages.IsValue(Key) then
    ViewGameMessages else
  if CastleKey_CancelFlying.IsValue(Key) then
    CancelFlying else
  if CastleKey_FPSShow.IsValue(Key) then
    ShowDebugInfo := not ShowDebugInfo else
  if CastleKey_Interact.IsValue(Key) then
    DoInteract else

  { Fixed keys. }
  if C = CharEscape then
    DoGameMenu;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;

procedure MouseDown(Glwin: TGLWindow; Btn: TMouseButton);
begin
  case Btn of
    mbLeft: DoAttack;
    mbRight: DoInteract;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  { Note that Glwin.SetMousePosition may (but doesn't have to)
    generate OnMouseMove to destination position.
    This can cause some problems:

    1. Consider this:

       - player moves mouse to MiddleX-10
       - MouseMove is generated, I rotate camera by "-10" horizontally
       - Glwin.SetMousePosition sets mouse to the Middle,
         but this time no MouseMove is generated
       - player moved mouse to MiddleX+10. Although mouse was
         positioned on Middle, TGLWindow thinks that the mouse
         is still positioned on Middle-10, and I will get "+20" move
         for player (while I should get only "+10")

       Fine solution for this would be to always subtract
       MiddleScreenWidth and MiddleScreenHeight below
       (instead of previous values, Glwin.MouseX and Glwin.MouseY).
       But this causes another problem:

    2. What if player switches to another window, moves the mouse,
       than goes alt+tab back to our window ? Next mouse move will
       be stupid, because it's really *not* from the middle of the screen.

    The solution for both problems: you have to check that previous
    position, Glwin.MouseX and Glwin.MouseY, are indeed equal to
    MiddleScreenWidth and MiddleScreenHeight. This way we know that
    this is good move, that qualifies to perform mouse move. }
  if (Glwin.MouseX = MiddleScreenWidth) and
     (Glwin.MouseY = MiddleScreenHeight) then
    Player.Navigator.MouseMove(
      NewX - MiddleScreenWidth, NewY - MiddleScreenHeight);

  { I check the condition below to avoid calling Glwin.SetMousePosition,
    OnMouseMove, Glwin.SetMousePosition, OnMouseMove... in a loop.
    Not really likely (as messages will be queued, and some
    SetMousePosition will finally just not generate OnMouseMove),
    but I want to safeguard anyway. }
  if (NewX <> MiddleScreenWidth) or (NewY <> MiddleScreenHeight) then
    Glwin.SetMousePosition(MiddleScreenWidth, MiddleScreenHeight);
end;

type
  TDummy = class
    class procedure MatrixChanged(Navigator: TMatrixNavigator);
  end;

class procedure TDummy.MatrixChanged(Navigator: TMatrixNavigator);
begin
  Glw.PostRedisplay;
  alUpdateListener;

  if Box3dPointInside(Player.Navigator.CameraPos, Level.AboveWaterBox) then
    Player.Swimming := psAboveWater else
  if Box3dPointInside(Player.Navigator.CameraPos, Level.WaterBox) then
    Player.Swimming := psUnderWater else
    Player.Swimming := psNo;
end;

procedure PlayGame(var ALevel: TLevel; APlayer: TPlayer;
  PrepareNewPlayer: boolean);
var
  SavedMode: TGLMode;
begin
  CreaturesKinds.PrepareRender;
  ItemsKinds.PrepareRender;
  TimeMessagesClear;

  GameWin := false;

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

        { OnTimer should be executed quite often, because footsteps sound
          (done in TPlayer.Idle) relies on the fact that OnUsingEnd
          of it's source will be called more-or-less immediately after
          sound stopped. And our Timer calls RefreshUsed that will
          call OnUsingEnd. }
        Glwm.TimerMilisec := 100;
        Glw.OnTimer := Timer;

        Glw.OnIdle := Idle;
        Glw.OnKeyDown := KeyDown;
        Glw.OnMouseDown := MouseDown;
        Glw.OnMouseMove := MouseMove;

        { Needed for mouse look }
        Glw.MouseVisible := false;
        Glw.SetMousePosition(MiddleScreenWidth, MiddleScreenHeight);

        InitNewLevel;

        GameEnded := false;

        glEnable(GL_LIGHTING);

        GLWinMessagesTheme.RectColor[3] := 0.4;

        if PrepareNewPlayer then
          Level.PrepareNewPlayer(Player);

        repeat
          Glwm.ProcessMessage(true);
        until GameEnded;
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

procedure LevelFinished(NextLevel: TLevel);
begin
  if NextLevel = nil then
  begin
    TimeMessage('Congratulations, game finished');
    GameWin := true;
    MusicPlayer.PlayedSound := stGameWinMusic;
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
  TimeMessage('Screen saved to ' + FileName);
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
    TimeMessagesManager.MaxMessagesCount }
  DarkAreaHeight = 80;

  DarkAreaFadeHeight = 20;
  DarkAreaAlpha = 0.3;
var
  I: Integer;
begin
  { Calculate GLList_Draw2dBegin }
  GLList_Draw2dBegin := glGenListsCheck(1, 'CastlePlay.GLWindowInit');
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
  ShowDebugInfo := false;
  Glw.OnInitList.AppendItem(@GLWindowInit);
  Glw.OnCloseList.AppendItem(@GLWindowClose);

  AutoOpenInventory := ConfigFile.GetValue(
    'auto_open_inventory', DefaultAutoOpenInventory);
finalization
  ConfigFile.SetDeleteValue('auto_open_inventory',
    AutoOpenInventory, DefaultAutoOpenInventory);
end.