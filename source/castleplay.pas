{
  Copyright 2006,2007 Michalis Kamburelis.

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

{ If NextLevel = '', then end the game,
  else free current Level and set Level to NextLevel.

  Note that this doesn't work immediately, but will perform
  at nearest possibility. While LevelFinished is scheduled but not
  performed yet, you of course can't call LevelFinished once again
  with different NextLevelName. }
procedure LevelFinished(NextLevelName: string);

{ If some LevelFinished call is scheduled, this will force changing
  level @italic(now). Don't use this, unless you know that you can
  safely change the level now (which means that old level will be
  destroyed, along with all it's items, creatures etc. references). }
procedure LevelFinishedFlush;

{ Saves a screen, causing also appropriate TimeMessage. }
procedure SaveScreen;

var
  { These fonts can be used globally by anything in this game.
    They are initialized in Glw.OnInit and finalized in Glw.OnClose in this unit. }
  Font_BFNT_BitstreamVeraSans_m10: TGLBitmapFont_Abstract;
  Font_BFNT_BitstreamVeraSans: TGLBitmapFont_Abstract;

var
  { Read-only from outside of this unit. }
  GameEnded: boolean;
  { This is important only if GameEnded. Will be a level name (<> '')
    if user wants to immediately restart the game. Caller of PlayGame
    should use this. }
  GameEndedWantsRestart: string;

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

var
  DebugRenderForLevelScreenshot: boolean = false;
  DebugTimeStopForCreatures: boolean = false;

implementation

uses Math, SysUtils, KambiUtils, GLWindow, KambiOpenAL, ALUtils,
  GLWinModes, GL, GLU, GLExt, KambiGLUtils, GLWinMessages, CastleWindow,
  Navigation, VectorMath, Boxes3d, Images,
  CastleHelp, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit,
  BFNT_BitstreamVeraSans_Unit, CastleCreatures,
  CastleItems, VRMLTriangleOctree, RaysWindow, KambiStringUtils,
  KambiFilesUtils, CastleInputs, CastleGameMenu, CastleDebugMenu, CastleSound,
  CastleVideoOptions, Keys, CastleConfig, VRMLGLHeadlight, CastleThunder,
  CastleTimeMessages, BackgroundGL, CastleControlsMenu,
  CastleLevelSpecific, VRMLGLScene, CastleLevelAvailable,
  ShadowVolumesHelper, KambiTimeUtils, GLImages;

var
  GLList_TimeMessagesBackground: TGLuint;

  GLList_InventorySlot: TGLuint;
  InventoryVisible: boolean;
  ShowDebugInfo: boolean;

  DisplayFpsUpdateTick: TMilisecTime;
  DisplayFpsFrameTime: Single;
  DisplayFpsRealTime: Single;

  LevelFinishedSchedule: boolean = false;
  { If LevelFinishedSchedule, then this is not-'', and should be the name
    of next Level to load. }
  LevelFinishedNextLevelName: string;

  SVHelper: TShadowVolumesHelper;

const
  SDeadMessage = 'You''re dead';
  SGameWinMessage = 'Game finished';

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

function ProjectionFar: Single;
begin
  if RenderShadowsPossible and RenderShadows then
    Result := ZFarInfinity else
    Result := Level.ProjectionFar;
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
    Level.ProjectionNear, ProjectionFar);

  UpdateNavigatorProjectionMatrix;
end;

procedure Draw2D(Draw2DData: Pointer);

  procedure DoDrawInventory;
  const
    InventorySlotWidth = 100;
    InventorySlotHeight = 100;
    InventorySlotMargin = 2;
  var
    InventorySlotsVisibleInColumn: Integer;

    function ItemSlotX(I: Integer): Integer;
    begin
      Result := Glw.Width - InventorySlotWidth *
        ((I div InventorySlotsVisibleInColumn) + 1);
    end;

    function ItemSlotY(I: Integer): Integer;
    begin
      Result := InventorySlotHeight * (InventorySlotsVisibleInColumn
        - 1 - (I mod InventorySlotsVisibleInColumn));
    end;

  var
    I, X, Y: Integer;
    S: string;
  begin
    InventorySlotsVisibleInColumn := Glw.Height div InventorySlotHeight;

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      { Draw at least InventorySlotsVisibleInColumn slots,
        possibly drawing empty slots. This is needed, because
        otherwise when no items are owned player doesn't see any
        effect of changing InventoryVisible. }
      for I := 0 to Max(Player.Items.Count - 1,
        InventorySlotsVisibleInColumn - 1) do
      begin
        X := ItemSlotX(I);
        Y := ItemSlotY(I);

        glRasterPos2i(X, Y);
        glCallList(GLList_InventorySlot);
      end;
    glDisable(GL_BLEND);

    glAlphaFunc(GL_GREATER, 0.5);
    glEnable(GL_ALPHA_TEST);
      for I := 0 to Player.Items.Count - 1 do
      begin
        X := ItemSlotX(I);
        Y := ItemSlotY(I);

        glRasterPos2i(X + InventorySlotMargin, Y + InventorySlotMargin);
        glCallList(Player.Items[I].Kind.GLList_DrawImage);
      end;
    glDisable(GL_ALPHA_TEST);

    if Between(Player.InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      glColor4f(0.8, 0.8, 0.8, 1);
      DrawGLRectBorder(
        ItemSlotX(Player.InventoryCurrentItem) + InventorySlotMargin,
        ItemSlotY(Player.InventoryCurrentItem) + InventorySlotMargin,
        ItemSlotX(Player.InventoryCurrentItem)
          + InventorySlotWidth - InventorySlotMargin,
        ItemSlotY(Player.InventoryCurrentItem)
          + InventorySlotHeight - InventorySlotMargin);
    end;

    glColor4f(1, 1, 0.5, 1);
    for I := 0 to Player.Items.Count - 1 do
    begin
      X := ItemSlotX(I);
      Y := ItemSlotY(I);

      glRasterPos2i(X + InventorySlotMargin, Y + InventorySlotMargin);

      S := Player.Items[I].Kind.Name;
      if Player.Items[I].Quantity <> 1 then
        S += ' (' + IntToStr(Player.Items[I].Quantity) + ')';
      Font_BFNT_BitstreamVeraSans_m10.Print(S);
    end;
  end;

  const
    { line number 1 is for "flying" text in Player.Draw2D }
    LineDeadOrWinner = 2;
    LinePressEscape = 3;
    LinePressAttack = 4;
    LineFPS = 5;
    LineShadowVolumesCounts = 6;

  procedure RasterPosLine(const Line: Cardinal);
  begin
    glRasterPos2i(0, Glw.Height -
      Font_BFNT_BitstreamVeraSans.RowHeight * Line - 10 { margin });
  end;

  procedure DoShowFPS;
  begin
    glColorv(Vector3Single(0.7, 0.7, 0.7));
    RasterPosLine(LineFPS);

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

  procedure DoShowShadowVolumesCounts;
  begin
    if RenderShadowsPossible and RenderShadows then
    begin
      glColorv(Vector3Single(0.7, 0.7, 0.7));
      RasterPosLine(LineShadowVolumesCounts);
      Font_BFNT_BitstreamVeraSans.Print(Format(
        'No shadow %d + zpass %d + zfail (no l cap) %d + zfail (l cap) %d = all %d',
        [ SVHelper.CountShadowsNotVisible,
          SVHelper.CountZPass,
          SVHelper.CountZFailNoLightCap,
          SVHelper.CountZFailAndLightCap,
          SVHelper.CountScenes ]));
    end;
  end;

  procedure DoShowDeadOrFinishedKeys;

    const
      SPressEscapeToExit = 'Press [Escape] to exit to menu.';

    function SPressAttackToRestart: string;
    begin
      Result := 'Press [Attack] (' +
        CastleInput_Attack.Shortcut.Description('not assigned') +
        ') to restart the level.';
    end;

  begin
    RasterPosLine(LinePressEscape);
    Font_BFNT_BitstreamVeraSans.Print(SPressEscapeToExit);
    RasterPosLine(LinePressAttack);
    Font_BFNT_BitstreamVeraSans.Print(SPressAttackToRestart);
  end;

  procedure DoShowDeadInfo;
  begin
    glColorv(Vector3Single(1, 0, 0));
    RasterPosLine(LineDeadOrWinner);
    Font_BFNT_BitstreamVeraSans.Print(SDeadMessage + '.');
    DoShowDeadOrFinishedKeys;
  end;

  procedure DoShowGameWinInfo;
  begin
    glColorv(Vector3Single(0.8, 0.8, 0.8));
    RasterPosLine(LineDeadOrWinner);
    Font_BFNT_BitstreamVeraSans.Print(SGameWinMessage + '.');
    DoShowDeadOrFinishedKeys;
  end;

begin
  Player.RenderWeapon2D;

  glLoadIdentity;
  glRasterPos2i(0, 0);

  if TimeMessagesDrawNeeded then
  begin
    glCallList(GLList_TimeMessagesBackground);
    TimeMessagesDraw;
  end;

  if InventoryVisible then
    DoDrawInventory;

  if ShowDebugInfo then
  begin
    DoShowFPS;
    DoShowShadowVolumesCounts;
  end;

  if Player.Dead then
    DoShowDeadInfo;

  if GameWin then
    DoShowGameWinInfo;

  Player.Render2D;
end;

procedure Draw(Glwin: TGLWindow);

  procedure RenderCreaturesItems(TransparentGroup: TTransparentGroup);
  begin
    { When GameWin, don't render creatures (as we don't check
      collisions when MovingPlayerEndSequence). }
    if not GameWin then
      Level.Creatures.Render(Player.Navigator.Frustum, TransparentGroup);
    if not DebugRenderForLevelScreenshot then
      Level.Items.Render(Player.Navigator.Frustum, TransparentGroup);
  end;

  procedure RenderNoShadows;
  begin
    RenderCreaturesItems(tgOpaque);
    Level.Render(Player.Navigator.Frustum);
    { Rendering order of Creatures, Items and Level:
      You know the problem. We must first render all non-transparent objects,
      then all transparent objects. Otherwise transparent objects
      (that must be rendered without updating depth buffer) could get brutally
      covered by non-transparent objects (that are in fact further away from
      the camera). }
    RenderCreaturesItems(tgTransparent);
  end;

  procedure RenderWithShadows;
  var
    MainLightPosition: TVector4Single;

    procedure RenderShadowVolume;
    var
      I: Integer;
    begin
      for I := 0 to Level.Creatures.High do
      begin
        Level.Creatures.Items[I].RenderShadowVolume(SVHelper);
      end;
      Level.RenderShadowVolume(SVHelper);
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
      front shadow quads from any possible camera view. Practically speaking,
      it will always be too little (for complicated shadow casters),
      but stencil_wrap will hopefully in this case minimize artifacts. }
    StencilShadowBits = $FF;
  begin
    { Creatures and items are never in shadow (this looks bad).
      So I render them here, when the lights are turned on
      and ignoring stencil buffer. They are rendered fully before
      any Level.Render --- since they are always opaque. }
    RenderCreaturesItems(tgOpaque);

    if Level.PushLightsOff(MainLightPosition) then
    begin
      try
        Level.Render(Player.Navigator.Frustum);
      finally Level.PopLightsOff; end;
    end else
    begin
      RenderNoShadows;
      Exit;
    end;

    SVHelper.InitFrustumAndLight(Player.Navigator.Frustum, MainLightPosition);
    SVHelper.Count := ShowDebugInfo;

    glEnable(GL_STENCIL_TEST);
      { Note that stencil buffer is set to all 0 now. }

      glPushAttrib(GL_ENABLE_BIT
        { saves Enable(GL_DEPTH_TEST), Enable(GL_CULL_FACE) });
        glEnable(GL_DEPTH_TEST);

        { Calculate shadows to the stencil buffer.
          Don't write anything to depth or color buffers. }
        glSetDepthAndColorWriteable(GL_FALSE);
          glStencilFunc(GL_ALWAYS, 0, 0);

          if SVHelper.StencilTwoSided then
          begin
            SVHelper.StencilSetupKind := ssFrontAndBack;
            RenderShadowVolume;
          end else
          begin
            glEnable(GL_CULL_FACE);

            { Render front facing shadow shadow volume faces. }
            SVHelper.StencilSetupKind := ssFront;
            glCullFace(GL_BACK);
            RenderShadowVolume;

            { Render back facing shadow shadow volume faces. }
            SVHelper.StencilSetupKind := ssBack;
            SVHelper.Count := false;
            glCullFace(GL_FRONT);
            RenderShadowVolume;
            SVHelper.Count := ShowDebugInfo;
          end;

        glSetDepthAndColorWriteable(GL_TRUE);
      glPopAttrib;
    glDisable(GL_STENCIL_TEST);

    { Now render everything once again, with lights turned on.
      But render only things not in shadow.

      ----------------------------------------
      Long (but maybe educational) explanation why glDepthFunc(GL_LEQUAL)
      below is crucial:

      What should I do with depth buffer ? Now it contains opaque
      never-shadowed things (creatures/items), and whole Level.Render.
      At some time (see revision 2048 in Kambi SVN repository on kocury) I called
      glClear(GL_DEPTH_BUFFER_BIT) before rendering level for the
      second time. While this seems to work, the 2nd pass leaves the depth-buffer
      untouched on shadowed places. So it creates a whole
      lot of problems for rendering never-shadowed things:
      1. I have to render them before glClear(GL_DEPTH_BUFFER_BIT),
         since they can be occluded by any level parts (shadowed on not).
      2. I have to render them once again (but only into depth buffer,
         i.e. with temporary
         glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE))
         after glClear(GL_DEPTH_BUFFER_BIT), since they can occlude
         non-shadowed level parts.

      This is easy doable for opaque parts. But what about transparent
      parts of never-shadowed things ? In other words, where should the
      call RenderCreaturesItems(tgTransparent) be done ?
      They should be rendered but they don't affect depth buffer.
      Well, clearly, they have to be rendered
      before glClear(GL_DEPTH_BUFFER_BIT) (for the same reason that
      opaque parts must: because they can be occluded by any level
      part, shadowed or not). But rendering non-shadowed parts may then
      cover them (since they cannot be rendered to depth buffer).
      So I should render them once again, but *only at the places
      where Level.Render below passed the stencil test (=0) and the depth test*.
      But the second condition is not so easy (I would have to change stencil
      buffer once again, based on Level.Render hits).

      The simpler version, to just render them second time everywhere where
      were not in shadow, i.e. stencil test (=0) passes would be no good:
      we would render transparent things twice in the places on the level
      when they are not in shadow.

      So basically, it's all doable, but not trivial, and (more important)
      I'm losing rendering time on handling this. And without taking proper care
      about transparent parts of never-shadowed things,
      transparent creatures/items will be visible through the walls,
      in the places where shadow falls on the wall.

      This was all talking assuming that we do glClear(GL_DEPTH_BUFFER_BIT)
      before the 2nd pass. But do we really have to ? No!
      It's enough to just set depth test GL_LEQUAL (instead of default
      GL_LESS) and then the 2nd pass will naturally cover the level
      rendered in the 1st pass. That's all, it's easy to implement,
      works perfectly, and is fast.

      End of long explanation about glDepthFunc(GL_LEQUAL).
      ----------------------------------------
    }

    glPushAttrib(GL_DEPTH_BUFFER_BIT { for glDepthFunc });
      glDepthFunc(GL_LEQUAL);

      { setup stencil : don't modify stencil, stencil test passes only for =0 }
      glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
      glStencilFunc(GL_EQUAL, 0, StencilShadowBits);
      glEnable(GL_STENCIL_TEST);
        Level.Render(Player.Navigator.Frustum);
      glDisable(GL_STENCIL_TEST);
    glPopAttrib();

    if CastleVideoOptions.DebugRenderShadowVolume then
    begin
      SVHelper.Count := false;
      glPushAttrib(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_ENABLE_BIT);
        glEnable(GL_DEPTH_TEST);
        glDisable(GL_LIGHTING);
        glColor4f(1, 1, 0, 0.3);
        glDepthMask(GL_FALSE);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
        RenderShadowVolume;
      glPopAttrib;
      SVHelper.Count := ShowDebugInfo;
    end;

    RenderCreaturesItems(tgTransparent);
  end;

var
  ClearBuffers: TGLbitfield;
  UsedBackground: TBackgroundGL;
begin
  ClearBuffers := GL_DEPTH_BUFFER_BIT;

  if RenderShadowsPossible and RenderShadows then
    ClearBuffers := ClearBuffers or GL_STENCIL_BUFFER_BIT;

  UsedBackground := Level.Background;

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
    RenderWithShadows else
    RenderNoShadows;

  Player.RenderAttack;

  if not DebugRenderForLevelScreenshot then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LIGHTING);
      glProjectionPushPopOrtho2D(@Draw2d, nil,
        0, Glw.Width, 0, Glw.Height);
    glPopAttrib;
  end;
end;

{ Call this when Level value changed (because of LevelFinished
  or because we just started new game). }
procedure InitNewLevel;
begin
  { Resize uses Level.BoundingBox to set good projection min/max depths.
    So we must call EventResize on each Level change. }
  Glw.EventResize;

  Player.LevelChanged;

  { Init Player.Navigator properties }
  Player.Navigator.OnMoveAllowed := @Level.PlayerMoveAllowed;
  Player.Navigator.OnGetCameraHeight := @Level.PlayerGetCameraHeightSqr;

  { Init initial camera pos }
  Player.Navigator.Init(Level.InitialCameraPos, Level.InitialCameraDir,
    Level.InitialCameraUp, Level.GravityUp, Level.CameraPreferredHeight,
    0.0 { Level.CameraPreferredHeight is already corrected if necessary,
          so I pass here 0.0 instead of CameraRadius } );

  Player.Navigator.CancelFallingDown;

  { Init Level.Headlight }
  TVRMLGLHeadlight.RenderOrDisable(Level.Headlight, 0);

  if Level.ThunderEffect <> nil then
    Level.ThunderEffect.InitGLLight(1);

  glLightModelv(GL_LIGHT_MODEL_AMBIENT, Level.GlobalAmbientLight);

  SoundEngine.MusicPlayer.PlayedSound := Level.PlayedMusicSound;

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
    PositionChangeSpeed = 0.05 * 50;
    DirectionChangeSpeed = 0.01 * 50;
    UpChangeSpeed = 0.01 * 50;
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

  procedure SickProjection;
  var
    S, C: Extended;
  begin
    SinCos(Level.AnimationTime * Player.SickProjectionSpeed, S, C);
    ProjectionGLPerspective(
      ViewAngleDegY + ViewAngleDegY * C * 0.03,
      Glwin.Width / Glwin.Height +
      Glwin.Width / Glwin.Height * S * 0.03,
      Level.ProjectionNear, ProjectionFar);
  end;

const
  GameWinPosition1: TVector3Single = (30.11, 146.27, 1.80);
  GameWinPosition2: TVector3Single = (30.11, 166.27, 1.80);
  GameWinDirection: TVector3Single = (0, 1, 0);
  GameWinUp: TVector3Single = (0, 0, 1);
var
  PickItemIndex: Integer;
begin
  CompSpeed := Glw.IdleSpeed;

  TimeMessagesIdle;

  Level.Idle(CompSpeed);
  Level.Items.Idle(CompSpeed);

  if Player.Swimming = psUnderWater then
    SickProjection;

  if (not GameWin) and (not DebugTimeStopForCreatures) then
    Level.Creatures.Idle(CompSpeed);
  Level.Creatures.RemoveFromLevel;

  if (not Player.Dead) and (not GameWin) then
  begin
    PickItemIndex := Level.Items.PlayerCollision;
    if PickItemIndex <> -1 then
    begin
      Player.PickItem(Level.Items[PickItemIndex].ExtractItem);
      Level.Items.FreeAndNil(PickItemIndex);
      Level.Items.Delete(PickItemIndex);

      if AutoOpenInventory then
        InventoryVisible := true;
    end;
  end;

  Player.Idle(CompSpeed);

  LevelFinishedFlush;

  if GameWin and (Level is TCagesLevel) then
  begin
    if TCagesLevel(Level).DoEndSequence then
      ModifyPlayerEndSequence(GameWinPosition2, GameWinDirection, GameWinUp) else
      ModifyPlayerEndSequence(GameWinPosition1, GameWinDirection, GameWinUp);
  end;
end;

procedure LevelFinishedFlush;
var
  NewLevel: TLevel;
begin
  if LevelFinishedSchedule then
  begin
    LevelFinishedSchedule := false;

    NewLevel := LevelsAvailable.FindName(LevelFinishedNextLevelName).CreateLevel;

    { First TLevel constructor was called.
      This actully loaded the level, displaying some progress bar.
      Background of this progress bar is our old Level --- so Level variable
      must stay valid and non-nil during loading of new level.
      Only after NewLevel is initialized, we quickly change Level variable. }
    FreeAndNil(Level);
    Level := NewLevel;

    InitNewLevel;
  end;
end;

procedure Timer(Glwin: TGLWindow);
begin
  if ALActive then
  begin
    CheckAL('game loop (check in OnTimer)');
    SoundEngine.ALRefreshUsedSources;
  end;
end;

procedure GameCancel(RequireConfirmation: boolean);
begin
  if Player.Dead or GameWin or (not RequireConfirmation) or
    MessageYesNo(Glw, 'Are you sure you want to end the game ?', taLeft) then
  begin
    GameEndedWantsRestart := '';
    GameEnded := true;
  end;
end;

procedure DoAttack;
begin
  if GameWin or Player.Dead then
  begin
    GameEndedWantsRestart := Level.Name;
    GameEnded := true;
  end else
    Player.Attack;
end;

procedure DoInteract;

  function TryInteract(RayVector: TVector3Single): boolean;
  type
    { TODO: This will be expanded with at least poEnemy in the future. }
    TPickedObjectType = (poNone, poLevel, poItem, poSpecialObject);
  var
    Ray0: TVector3Single;
    ItemCollisionIndex, SpecialObjectIndex, I: integer;
    IntersectionDistance, ThisIntersectionDistance: Single;
    PickedObjectType: TPickedObjectType;
    LevelCollisionObjectIndex: Integer;
    LevelCollisionInfo: TCollisionInfo;
  begin
    Ray0 := Player.Navigator.CameraPos;

    { Picking is not an often called procedure, so I can freely normalize
      here to get exact distance to picked object in IntersectionDistance. }
    NormalizeTo1st(RayVector);

    IntersectionDistance := MaxSingle;
    PickedObjectType := poNone;

    { Now start picking, by doing various tests for collisions with ray
      (Ray0, RayVector). The pick that has smallest IntersectionDistance
      "wins". }

    { Collision with Level (Scene and Objects) }
    LevelCollisionInfo := Level.TryPick(
      ThisIntersectionDistance, LevelCollisionObjectIndex, Ray0, RayVector);
    if (LevelCollisionInfo <> nil) and
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
          Result := false;
          Level.Picked(IntersectionDistance,
            LevelCollisionInfo, LevelCollisionObjectIndex, Result);
        end;
      poItem:
        begin
          Level.Items[ItemCollisionIndex].ItemPicked(IntersectionDistance);
          Result := true;
        end;
      poSpecialObject:
        begin
          Result := false;
          Level.SpecialObjectPicked(
            IntersectionDistance, SpecialObjectIndex, Result);
        end;
      else
        Result := false;
    end;

    { No matter what happened, remember to always free LevelCollisionInfo }
    FreeAndNil(LevelCollisionInfo);
  end;

  function TryInteractAround(const XChange, YChange: Integer): boolean;
  var
    RayVector: TVector3Single;
  begin
    RayVector := PrimaryRay(
      Glw.Width div 2 + XChange,
      Glw.Height div 2 + YChange,
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
            SoundEngine.Sound(stPlayerInteractFailed);
end;

procedure MaybeDeadWinMessage;
begin
  if GameWin then
    TimeMessage(SGameWinMessage) else
  if Player.Dead then
    TimeMessage(SDeadMessage);
end;

{ Call this always when entering the game mode, or when UseMouseLook changes
  while we're in game mode. This sets mouse visibility and position. }
procedure UpdateMouseLook;
begin
  { Glwin.UpdateMouseLook will read Navigator.MouseLook, so we better set
    it here (even though it's set in Player.Idle). }
  Player.Navigator.MouseLook := UseMouseLook;
  Glw.UpdateMouseLook;
end;

procedure EventDown(MouseEvent: boolean; Key: TKey;
  AMouseButton: TMouseButton);

  procedure ChangeInventoryCurrentItem(Change: Integer);
  begin
    if Player.Items.Count = 0 then
      Player.InventoryCurrentItem := -1 else
    if Player.InventoryCurrentItem >= Player.Items.Count then
      Player.InventoryCurrentItem := Player.Items.Count - 1 else
    if Player.InventoryCurrentItem < 0 then
      Player.InventoryCurrentItem := 0 else
      Player.InventoryCurrentItem := ChangeIntCycle(
        Player.InventoryCurrentItem, Change, Player.Items.Count - 1);
  end;

  procedure UpdateInventoryCurrentItemAfterDelete;
  begin
    { update InventoryCurrentItem.
      Note that if Player.Items.Count = 0 now, then this will
      correctly set InventoryCurrentItem to -1. }
    if Player.InventoryCurrentItem >= Player.Items.Count then
      Player.InventoryCurrentItem := Player.Items.Count - 1;
  end;

  procedure DropItem;

    function GetItemDropPosition(
      DroppedItemKind: TItemKind;
      out DropPosition: TVector3Single): boolean;
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

        Note that I take PushVector from CameraDirInGravityPlane,
        not from CameraDir, otherwise when player is looking
        down he could be able to put item "inside the ground".
        Collision detection with the level below would actually
        prevent putting item "inside the ground", but the item
        would be too close to the player --- he could pick it up
        immediately. }
      PushVector := Player.Navigator.CameraDirInGravityPlane;
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

    if Between(Player.InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      if GetItemDropPosition(Player.Items[Player.InventoryCurrentItem].Kind,
        DropPosition) then
      begin
        DropppedItem := Player.DropItem(Player.InventoryCurrentItem);
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

    if Between(Player.InventoryCurrentItem, 0, Player.Items.Count - 1) then
    begin
      UsedItem := Player.Items[Player.InventoryCurrentItem];
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

  procedure UseLifePotion;
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

    UsedItemIndex := Player.Items.FindKind(LifePotion);
    if UsedItemIndex <> -1 then
    begin
      UsedItem := Player.Items[UsedItemIndex];
      UsedItem.Kind.Use(UsedItem);
      if UsedItem.Quantity = 0 then
      begin
        { I seek for UsedItemIndex once again, because using item
          could change item indexes. }
        UsedItemIndex := Player.Items.IndexOf(UsedItem);
        if UsedItemIndex <> -1 then
          Player.DeleteItem(UsedItemIndex).Free;
      end;

      UpdateInventoryCurrentItemAfterDelete;
    end else
      TimeMessage('You don''t have any life potion');
  end;

  procedure CancelFlying;
  begin
    if GameWin then
      TimeMessage(SGameWinMessage) else
    if not Player.Dead then
      Player.CancelFlying else
      TimeMessage(SDeadMessage);
  end;

  procedure MaybeWinMessage;
  begin
    if GameWin then
      TimeMessage(SGameWinMessage);
  end;

  procedure DoDebugMenu;
  begin
    ShowDebugMenu(@Draw);
  end;

begin
  { Basic keys. }
  if CastleInput_Attack.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    DoAttack else
  if CastleInput_UpMove.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_DownMove.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_Forward.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_Backward.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_LeftStrafe.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_RightStrafe.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    MaybeDeadWinMessage else
  if { Note that rotation keys work even when player is dead.
       See comments in TPlayer.UpdateNavigator. }
     CastleInput_LeftRot.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_RightRot.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_UpRotate.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_DownRotate.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) or
     CastleInput_GravityUp.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    MaybeWinMessage else

  { Items keys. }
  if CastleInput_InventoryShow.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    InventoryVisible := not InventoryVisible else
  if CastleInput_InventoryPrevious.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    ChangeInventoryCurrentItem(-1) else
  if CastleInput_InventoryNext.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    ChangeInventoryCurrentItem(+1) else
  if CastleInput_DropItem.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    DropItem else
  if CastleInput_UseItem.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    UseItem else
  if CastleInput_UseLifePotion.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    UseLifePotion else

  { Other keys. }
  if CastleInput_SaveScreen.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    SaveScreen else
  if CastleInput_ViewMessages.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    ViewGameMessages else
  if CastleInput_CancelFlying.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    CancelFlying else
  if CastleInput_FPSShow.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    ShowDebugInfo := not ShowDebugInfo else
  if CastleInput_Interact.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    DoInteract else
  if CastleInput_DebugMenu.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    DoDebugMenu;
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);

  procedure DoGameMenu;
  begin
    ShowGameMenu(@Draw);
    { UseMouseLook possibly changed now. }
    UpdateMouseLook;
  end;

begin
  EventDown(false, Key, mbLeft);
  if C = CharEscape then
  begin
    if Player.Dead or GameWin then
      GameCancel(false) else
      DoGameMenu;
  end;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(true, K_None, Button);
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;

type
  TPlayGameHelper = class
    class procedure MatrixChanged(Navigator: TNavigator);
  end;

class procedure TPlayGameHelper.MatrixChanged(Navigator: TNavigator);
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
  PlayGameHelper: TPlayGameHelper;
begin
  TimeMessagesClear;

  GameWin := false;

  Level := ALevel;
  Player := APlayer;
  InventoryVisible := false;
  LevelFinishedSchedule := false;
  try

    SavedMode := TGLMode.Create(glw,
      { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
      GL_ENABLE_BIT, true);
    try
      { init navigator }
      Glw.Navigator := Player.Navigator;
      try
        { Init Player.Navigator properties }
        PlayGameHelper := nil;
        { No need to actually create PlayGameHelper class,
          but I must pass here an instance, not a TPlayGameHelper
          only --- at least in objfpc mode, see
          [http://lists.freepascal.org/lists/fpc-devel/2006-March/007370.html] }
        Player.Navigator.OnMatrixChanged := @PlayGameHelper.MatrixChanged;

        { Note that this sets AutoRedisplay to true. }
        SetStandardGLWindowState(Glw, @Draw, @CloseQuery, @Resize,
          nil, { AutoRedisplay } true, { FPSActive } true, { MenuActive } false,
          K_None, #0, { FpsShowOnCaption } false, { UseNavigator } true);

        { OnTimer should be executed quite often, because footsteps sound
          (done in TPlayer.Idle) relies on the fact that OnUsingEnd
          of it's source will be called more-or-less immediately after
          sound stopped. And our Timer calls RefreshUsed that will
          call OnUsingEnd. }
        Glwm.TimerMilisec := 100;
        Glw.OnTimer := @Timer;

        Glw.OnIdle := @Idle;
        Glw.OnKeyDown := @KeyDown;
        Glw.OnMouseDown := @MouseDown;

        UpdateMouseLook;

        InitNewLevel;

        GameEnded := false;
        GameEndedWantsRestart := '';

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

procedure LevelFinished(NextLevelName: string);
begin
  if NextLevelName = '' then
  begin
    TimeMessage('Congratulations, game finished');
    GameWin := true;
    SoundEngine.MusicPlayer.PlayedSound := stGameWinMusic;
  end else
  begin
    if LevelFinishedSchedule and
      (LevelFinishedNextLevelName <> NextLevelName) then
      raise EInternalError.Create(
        'You cannot call LevelFinished while previous LevelFinished is not done yet');

    LevelFinishedSchedule := true;
    LevelFinishedNextLevelName := NextLevelName;
  end;
end;

procedure SaveScreen;
var
  FileName: string;
begin
  FileName := FnameAutoInc(ApplicationName + '_screen_%d.png');
  Glw.SaveScreen(FileName);
  TimeMessage('Screen saved to ' + FileName);
  SoundEngine.Sound(stSaveScreen);
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
    Result := LoadImageToDisplayList(
      PlayerControlFileName(BaseName), [TAlphaImage], [], 0, 0);
  end;

  procedure InitializeShadows;
  begin
    SVHelper := TShadowVolumesHelper.Create;
    SVHelper.InitGLContext;
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
  { Calculate GLList_TimeMessagesBackground }
  GLList_TimeMessagesBackground := glGenListsCheck(1, 'CastlePlay.GLWindowInit');
  glNewList(GLList_TimeMessagesBackground, GL_COMPILE);
  try
    glLoadIdentity;
    glRasterPos2i(0, 0);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, DarkAreaAlpha);
      glRecti(0, 0, Glw.Width, DarkAreaHeight);
      for I := 0 to DarkAreaFadeHeight - 1 do
      begin
        glColor4f(0, 0, 0,
          DarkAreaAlpha * (DarkAreaFadeHeight - 1 - I) / DarkAreaFadeHeight);
        glRecti(0, DarkAreaHeight + I, Glw.Width, DarkAreaHeight + I + 1);
      end;
    glDisable(GL_BLEND);
  finally glEndList end;

  GLList_InventorySlot := LoadPlayerControlToDisplayList('item_slot.png');

  Font_BFNT_BitstreamVeraSans_m10 := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans_m10);
  Font_BFNT_BitstreamVeraSans     := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);

  InitializeShadows;
end;

procedure GLWindowClose(Glwin: TGLWindow);
begin
  FreeAndNil(Font_BFNT_BitstreamVeraSans);
  FreeAndNil(Font_BFNT_BitstreamVeraSans_m10);

  glFreeDisplayList(GLList_TimeMessagesBackground);
  glFreeDisplayList(GLList_InventorySlot);

  FreeAndNil(SVHelper);
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