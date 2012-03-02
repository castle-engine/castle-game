{
  Copyright 2006-2012 Michalis Kamburelis.

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

{ Playing the game. }

unit GamePlay;

interface

uses Classes, GameLevel, GamePlayer, OpenGLFonts;

{ Play the game.

  Level and Player global variables must be already initialized.
  Note that TLevel must be created after Player is initialized.

  Note that Level may change during the PlayGame, because
  of LevelFinished. In such case old Level value will be freeed,
  and Level will be set to new value. Last Level value should
  be freed by the caller of PlayGame.

  If PrepareNewPlayer then it will call Level.PrepareNewPlayer
  right before starting the actual game. }
procedure PlayGame(PrepareNewPlayer: boolean);

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
  with different NextLevelId. }
procedure LevelFinished(NextLevelId: string);

{ If some LevelFinished call is scheduled, this will force changing
  level @italic(now). Don't use this, unless you know that you can
  safely change the level now (which means that old level will be
  destroyed, along with all it's items, creatures etc. references). }
procedure LevelFinishedFlush;

{ Saves a screen, causing also appropriate Notification. }
procedure SaveScreen;

var
  { These fonts can be used globally by anything in this game.
    They are initialized in Window.OnInit and finalized in Window.OnClose in this unit. }
  Font_BFNT_BitstreamVeraSans_m10: TGLBitmapFont_Abstract;
  Font_BFNT_BitstreamVeraSans: TGLBitmapFont_Abstract;

var
  { Read-only from outside of this unit. }
  GameEnded: boolean;
  { Will be a level name (<> '') if user wants to immediately restart the game.
    This is important only if GameEnded.
    Caller of PlayGame should use this. }
  GameEndedWantsRestart: string;

  { Read-only from outside of this unit. Initially false when starting
    PlayGame. }
  GameWin: boolean;

{ Note that when Player.Dead or GameWin,
  confirmation will never be required anyway. }
procedure GameCancel(RequireConfirmation: boolean);

var
  DebugRenderForLevelScreenshot: boolean = false;

implementation

uses SysUtils, CastleUtils, CastleWindow,
  WindowModes, GL, GLU, GLExt, CastleGLUtils, CastleMessages, GameWindow,
  VectorMath, Boxes3D, Images,
  GameHelp, OpenGLBmpFonts, BFNT_BitstreamVeraSans_m10_Unit,
  BFNT_BitstreamVeraSans_Unit, UIControls,
  GameItems, CastleStringUtils,
  CastleFilesUtils, GameInputs, GameGameMenu, GameDebugMenu, GameSound,
  GameVideoOptions, GameConfig,
  GameNotifications, GameControlsMenu,
  GameLevelSpecific, GameLevelAvailable,
  CastleTimeUtils, GLImages, KeysMouse;

var
  GLList_NotificationsBackground: TGLuint;

  GLList_InventorySlot: TGLuint;

  DisplayFpsUpdateTick: TMilisecTime;
  DisplayFpsFrameTime: Single;
  DisplayFpsRealTime: Single;

  ShowDebugInfo: boolean;

  LevelFinishedSchedule: boolean = false;
  { If LevelFinishedSchedule, then this is not-'', and should be the name
    of next Level to load. }
  LevelFinishedNextLevelId: string;

  GameControls: TUIControlList;

const
  SDeadMessage = 'You''re dead';
  SGameWinMessage = 'Game finished';

type
  TGame2DControls = class(TUIControl)
  public
    procedure Draw; override;
    function DrawStyle: TUIControlDrawStyle; override;
  end;

function TGame2DControls.DrawStyle: TUIControlDrawStyle;
begin
  Result := ds2D;
end;

procedure TGame2DControls.Draw;

  procedure DoDrawInventory;
  const
    InventorySlotWidth = 100;
    InventorySlotHeight = 100;
    InventorySlotMargin = 2;
  var
    InventorySlotsVisibleInColumn: Integer;

    function ItemSlotX(I: Integer): Integer;
    begin
      Result := Window.Width - InventorySlotWidth *
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
    InventorySlotsVisibleInColumn := Window.Height div InventorySlotHeight;

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

      S := Player.Items[I].Kind.Caption;
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
    glRasterPos2i(0, Window.Height -
      Font_BFNT_BitstreamVeraSans.RowHeight * Line - 10 { margin });
  end;

  procedure DoShowFPS;
  begin
    glColorv(Vector3Single(0.7, 0.7, 0.7));
    RasterPosLine(LineFPS);

    { Don't display precise Window.FpsFrameTime and Window.FpsRealTime
      each time --- this would cause too much move for player.
      Instead, display DisplayFpsXxxTime that are updated each second. }
    if (DisplayFpsUpdateTick = 0) or
       (TimeTickDiff(DisplayFpsUpdateTick, GetTickCount) >= 1000) then
    begin
      DisplayFpsUpdateTick := GetTickCount;
      DisplayFpsFrameTime := Window.Fps.FrameTime;
      DisplayFpsRealTime := Window.Fps.RealTime;
    end;

    Font_BFNT_BitstreamVeraSans.Print(
      Format('FPS : %f (real : %f). Shapes : %d / %d',
        [DisplayFpsFrameTime, DisplayFpsRealTime,
         Level.Statistics.ShapesRendered, Level.Statistics.ShapesVisible]));
  end;

  procedure DoShowShadowVolumesCounts;
  begin
    if RenderShadowsPossible and RenderShadows then
    begin
      glColorv(Vector3Single(0.7, 0.7, 0.7));
      RasterPosLine(LineShadowVolumesCounts);
      Font_BFNT_BitstreamVeraSans.Print(Format(
        'No shadow %d + zpass %d + zfail (no l cap) %d + zfail (l cap) %d = all %d',
        [ Level.ShadowVolumeRenderer.CountShadowsNotVisible,
          Level.ShadowVolumeRenderer.CountZPass,
          Level.ShadowVolumeRenderer.CountZFailNoLightCap,
          Level.ShadowVolumeRenderer.CountZFailAndLightCap,
          Level.ShadowVolumeRenderer.CountScenes ]));
    end;
  end;

  procedure DoShowDeadOrFinishedKeys;

    const
      SPressEscapeToExit = 'Press [Escape] to exit to menu.';

    function SPressAttackToRestart: string;
    begin
      Result := 'Press [Interact] (' +
        CastleInput_Interact.Shortcut.Description('not assigned') +
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
  if DebugRenderForLevelScreenshot then Exit;

  glLoadIdentity;
  glRasterPos2i(0, 0);

  if Notifications.DrawStyle <> dsNone then
    glCallList(GLList_NotificationsBackground);

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

{ Call this when Level value changed (because of LevelFinished
  or because we just started new game). }
procedure InitNewLevel;
begin
  { No need to explicitly call any Window.EventResize or Level.ApplyProjection,
    newly added to Window.Controls (and possibly newly created) level will have
    ApplyProjectionNeeded := true. }

  Player.LevelChanged;

  SoundEngine.MusicPlayer.PlayedSound := Level.PlayedMusicSound;

  { First Notification for this level. }
  Notifications.Show('Loaded level "' + Level.Title + '"');
end;

procedure Idle(Window: TCastleWindowBase);
const
  GameWinPosition1: TVector3Single = (30.11, 146.27, 1.80);
  GameWinPosition2: TVector3Single = (30.11, 166.27, 1.80);
  GameWinDirection: TVector3Single = (0, 1, 0);
  GameWinUp: TVector3Single = (0, 0, 1);
var
  Cages: TCagesLevel;
begin
  Level.SickProjection := Player.Swimming = psUnderWater;
  if Level.SickProjection then
    Level.SickProjectionSpeed := Player.SickProjectionSpeed;

  LevelFinishedFlush;

  if GameWin and (Level is TCagesLevel) then
  begin
    Cages := TCagesLevel(Level);
    case Cages.GameWinAnimation of
      gwaNone:
        begin
          Assert(not Player.Camera.Animation);
          Player.Camera.AnimateTo(GameWinPosition1, GameWinDirection, GameWinUp, 4);
          Cages.GameWinAnimation := Succ(Cages.GameWinAnimation);
        end;
      gwaAnimateTo1:
        if not Player.Camera.Animation then
        begin
          SoundEngine.Sound(stKeyDoorUse);
          Player.Camera.AnimateTo(GameWinPosition2, GameWinDirection, GameWinUp, 4);
          Cages.GameWinAnimation := Succ(Cages.GameWinAnimation);
        end;
      gwaAnimateTo2:
        if not Player.Camera.Animation then
          Cages.GameWinAnimation := Succ(Cages.GameWinAnimation);
    end;
  end;
end;

procedure LevelFinishedFlush;
var
  NewLevel: TLevel;
  GameControlsPos, WindowControlsPos: Integer;
begin
  if LevelFinishedSchedule then
  begin
    LevelFinishedSchedule := false;

    { First, we have to disconnect Player and Camera from old Level.
      That is because when creating new level, Player and Camera must be connected
      with new level in it's constructor (that's how our TLevel.Create works),
      and you should not keep the same T3D (Player) and TCamera (Player.Camera)
      within different scene managers --- it would cause troubles with
      connecting callbacks to Camera, and Player.Parent. }
    Level.Camera := nil;
    Level.Player := nil;
    Level.Items.Remove(Player);

    { We cannot draw old level now, since it's Camera is nil,
      and TLevel.ApplyProjection is not prepared for this.
      So remove it from controls (otherwise progress bar when loading new level
      would want to draw it). }
    GameControlsPos := GameControls.IndexOf(Level);
    if GameControlsPos <> -1 then
      GameControls.Delete(GameControlsPos) else
      GameControlsPos := GameControls.Count;
    WindowControlsPos := Window.Controls.IndexOf(Level);
    if WindowControlsPos <> -1 then
      Window.Controls.Delete(WindowControlsPos) else
      WindowControlsPos := Window.Controls.Count;

    NewLevel := LevelsAvailable.FindId(LevelFinishedNextLevelId).CreateLevel;

    { copy DisableContextOpenClose value to new level.
      This is needed when it's called from inside debug menu,
      to make Window.Controls.Begin/EndDisableContextOpenClose
      matching. }
    NewLevel.DisableContextOpenClose := Level.DisableContextOpenClose;

    { initialize NewLevel.GLContextOpen already, in case this is called
      from inside debug menu (where explicit GLContextOpen may be disabled). }
    NewLevel.GLContextOpen;

    { right before freeing old Level, insert NewLevel at the same place
      in GameControls and Window.Controls as Level was. }
    GameControls.Insert(GameControlsPos, NewLevel);
    Window.Controls.Insert(WindowControlsPos, NewLevel);

    { After NewLevel is initialized, we quickly change Level variable. }
    FreeAndNil(Level);
    Level := NewLevel;

    InitNewLevel;
  end;
end;

procedure Timer(Window: TCastleWindowBase);
begin
  if SoundEngine.ALActive then
    SoundEngine.RefreshUsedSources;
end;

procedure GameCancel(RequireConfirmation: boolean);
begin
  if Player.Dead or GameWin or (not RequireConfirmation) or
    MessageYesNo(Window, 'Are you sure you want to end the game ?', taLeft) then
  begin
    GameEndedWantsRestart := '';
    GameEnded := true;
  end;
end;

procedure DoAttack;
begin
  if GameWin then
  begin
    Notifications.Show(SGameWinMessage);
    Exit;
  end;

  if Player.Dead then
  begin
    Notifications.Show(SDeadMessage);
    Exit;
  end;

  Player.Attack;
end;

procedure MaybeDeadWinMessage;
begin
  if GameWin then
    Notifications.Show(SGameWinMessage) else
  if Player.Dead then
    Notifications.Show(SDeadMessage);
end;

{ Call this always when entering the game mode, or when UseMouseLook changes
  while we're in game mode. }
procedure UpdateMouseLook;
begin
  { Set Camera.MouseLook now, even though it's set in Player.Idle. }
  Player.Camera.MouseLook := UseMouseLook;
end;

procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);

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

    if Player.Items.Count <> 0 then
      InventoryVisible := true;
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

    function GetItemDropPosition(DroppedItemKind: TItemKind;
      out DropPosition: TVector3Single): boolean;
    var
      ItemBox: TBox3D;
      ItemBoxRadius: Single;
      ItemBoxMiddle: TVector3Single;
    begin
      ItemBox := DroppedItemKind.BoundingBoxRotated;
      ItemBoxMiddle := ItemBox.Middle;
      { Box3DRadius calculates radius around (0, 0, 0) and we want
        radius around ItemBoxMiddle }
      ItemBoxRadius := ItemBox.Translate(VectorNegate(ItemBoxMiddle)).Radius;

      { Calculate DropPosition.

        We must move the item a little before us to
        1. show visually player that the item was dropped
        2. to avoid automatically picking it again

        Note that I take direction from DirectionInGravityPlane,
        not from Direction, otherwise when player is looking
        down he could be able to put item "inside the ground".
        Collision detection with the level below would actually
        prevent putting item "inside the ground", but the item
        would be too close to the player --- he could pick it up
        immediately. }
      DropPosition := Player.Camera.Position +
        Player.Camera.DirectionInGravityPlane * Sqrt2 * Max(
          Player.Camera.RealPreferredHeight,
          ItemBox.SizeX * 2, ItemBox.SizeY * 2);

      { Now check is DropPosition actually possible
        (i.e. check collisions item<->everything).
        The assumption is that item starts from
        Player.Camera.Position and is moved to DropPosition.

        But actually we must shift both these positions,
        so that we check positions that are ideally in the middle
        of item's BoundingBoxRotated. Otherwise the item
        could get *partially* stuck within the wall, which wouldn't
        look good. }

      Result := Level.Items.WorldMoveAllowed(
        VectorAdd(Player.Camera.Position, ItemBoxMiddle),
        VectorAdd(DropPosition, ItemBoxMiddle),
        true, ItemBoxRadius,
        ItemBox.Translate(Player.Camera.Position),
        ItemBox.Translate(DropPosition), false);
    end;

  var
    DropppedItem: TItem;
    DropPosition: TVector3Single;
  begin
    if GameWin then
    begin
      Notifications.Show(SGameWinMessage);
      Exit;
    end;

    if Player.Dead then
    begin
      Notifications.Show(SDeadMessage);
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
          Level.Items.Add(TItemOnLevel.Create(Level, DropppedItem, DropPosition));
        end;
      end else
        Notifications.Show('Not enough room here to drop this item');
    end else
      Notifications.Show('Nothing to drop - select some item first');
  end;

  procedure UseItem;
  var
    UsedItem: TItem;
    UsedItemIndex: Integer;
  begin
    if GameWin then
    begin
      Notifications.Show(SGameWinMessage);
      Exit;
    end;

    if Player.Dead then
    begin
      Notifications.Show(SDeadMessage);
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
      Notifications.Show('Nothing to use - select some item first');
  end;

  procedure UseLifePotion;
  var
    UsedItem: TItem;
    UsedItemIndex: Integer;
  begin
    if GameWin then
    begin
      Notifications.Show(SGameWinMessage);
      Exit;
    end;

    if Player.Dead then
    begin
      Notifications.Show(SDeadMessage);
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
      Notifications.Show('You don''t have any life potion');
  end;

  procedure CancelFlying;
  begin
    if GameWin then
      Notifications.Show(SGameWinMessage) else
    if not Player.Dead then
      Player.CancelFlying else
      Notifications.Show(SDeadMessage);
  end;

  procedure MaybeWinMessage;
  begin
    if GameWin then
      Notifications.Show(SGameWinMessage);
  end;

  procedure DoDebugMenu;
  begin
    { We explicitly clear, and later readd GameControls to the Window.Controls.
      Reason? During debug menu, current Level instance may change
      (because debug menu may change the level, and even call LevelFinishedFlush).
      During LevelFinishedFlush old Level instance is destroyed,
      and new TLevel created, and the lists GameControls and Window.Controls
      are updated... but our current Window.Controls list would be, at this time,
      only saved in TGLMode state, so it would still contain invalid pointer
      to the old level. So we should instead explicitly push/pop our current
      Window.Controls, this way using current GameControls value. }
    Window.Controls.BeginDisableContextOpenClose;
    Window.Controls.Clear;

    Level.Paused := true;
    ShowDebugMenu(GameControls);
    Level.Paused := false;

    Window.Controls.AddList(GameControls);
    Window.Controls.EndDisableContextOpenClose;
  end;

  procedure DoInteract;
  begin
    { normal interaction is already handled because
      TCastleSceneManager.Input_PointingDeviceActivate is equal to interact key. }
    if GameWin or Player.Dead then
    begin
      GameEndedWantsRestart := Level.Id;
      GameEnded := true;
    end;
  end;

begin
  { Basic keys. }
  if CastleInput_Attack.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    DoAttack else
  if CastleInput_UpMove.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_DownMove.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_Forward.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_Backward.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_LeftStrafe.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_RightStrafe.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    MaybeDeadWinMessage else
  if { Note that rotation keys work even when player is dead.
       See comments in TPlayer.UpdateCamera. }
     CastleInput_LeftRot.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_RightRot.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_UpRotate.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_DownRotate.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) or
     CastleInput_GravityUp.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    MaybeWinMessage else

  { Items keys. }
  if CastleInput_InventoryShow.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    InventoryVisible := not InventoryVisible else
  if CastleInput_InventoryPrevious.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    ChangeInventoryCurrentItem(-1) else
  if CastleInput_InventoryNext.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    ChangeInventoryCurrentItem(+1) else
  if CastleInput_DropItem.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    DropItem else
  if CastleInput_UseItem.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    UseItem else
  if CastleInput_UseLifePotion.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    UseLifePotion else

  { Other keys. }
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen else
  if CastleInput_ViewMessages.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    ViewGameMessages else
  if CastleInput_CancelFlying.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    CancelFlying else
  if CastleInput_FPSShow.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    ShowDebugInfo := not ShowDebugInfo else
  if CastleInput_Interact.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    DoInteract else
  if CastleInput_DebugMenu.Shortcut.IsEvent(AKey, #0, AMousePress, AMouseButton, AMouseWheel) then
    DoDebugMenu;
end;

procedure KeyDown(Window: TCastleWindowBase; Key: TKey; C: char);
begin
  EventDown(Key, false, mbLeft, mwNone);
  if C = CharEscape then
  begin
    if Player.Dead or GameWin then
      GameCancel(false) else
    begin
      Level.Paused := true;
      ShowGameMenu(GameControls);
      Level.Paused := false;
    end;
  end;
end;

procedure MouseDown(Window: TCastleWindowBase; Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
end;

procedure MouseWheel(Window: TCastleWindowBase; const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  GameCancel(true);
end;

procedure PlayGame(PrepareNewPlayer: boolean);
var
  SavedMode: TGLMode;
  C2D: TGame2DControls;
begin
  Notifications.Clear;

  GameWin := false;

  InventoryVisible := false;
  LevelFinishedSchedule := false;

  SavedMode := TGLMode.CreateReset(Window,
    { For glEnable(GL_LIGHTING) and GL_LIGHT0 below.}
    GL_ENABLE_BIT, true, nil, nil, @CloseQuery);
  try
    Window.AutoRedisplay := true;

    { OnTimer should be executed quite often, because footsteps sound
      (done in TPlayer.Idle) relies on the fact that OnUsingEnd
      of it's source will be called more-or-less immediately after
      sound stopped. And our Timer calls RefreshUsed that will
      call OnUsingEnd. }
    Application.TimerMilisec := 100;
    Window.OnTimer := @Timer;

    Window.OnIdle := @Idle;
    Window.OnKeyDown := @KeyDown;
    Window.OnMouseDown := @MouseDown;
    Window.OnMouseWheel := @MouseWheel;
    Window.OnDrawStyle := ds3D;

    C2D := TGame2DControls.Create(nil);
    GameControls := TUIControlList.CreateFromArray(false, [Notifications, C2D, Level]);

    Window.Controls.AddList(GameControls);

    InitNewLevel;

    GameEnded := false;
    GameEndedWantsRestart := '';

    glEnable(GL_LIGHTING);

    MessagesTheme.RectColor[3] := 0.4;

    if PrepareNewPlayer then
      Level.PrepareNewPlayer(Player);

    repeat
      Application.ProcessMessage(true, true);
    until GameEnded;
  finally
    { Clear some Player.Camera callbacks. }
    Level.OnCameraChanged := nil;

    FreeAndNil(GameControls);
    FreeAndNil(C2D);
    FreeAndNil(SavedMode);
  end;
end;

procedure LevelFinished(NextLevelId: string);
begin
  if NextLevelId = '' then
  begin
    Notifications.Show('Congratulations, game finished');
    GameWin := true;
    SoundEngine.MusicPlayer.PlayedSound := stGameWinMusic;
  end else
  begin
    if LevelFinishedSchedule and
      (LevelFinishedNextLevelId <> NextLevelId) then
      raise EInternalError.Create(
        'You cannot call LevelFinished while previous LevelFinished is not done yet');

    LevelFinishedSchedule := true;
    LevelFinishedNextLevelId := NextLevelId;
  end;
end;

procedure SaveScreen;
var
  FileName: string;
begin
  FileName := FileNameAutoInc(ApplicationName + '_screen_%d.png');
  Window.SaveScreen(FileName);
  Notifications.Show('Screen saved to ' + FileName);
  SoundEngine.Sound(stSaveScreen);
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(Window: TCastleWindowBase);

  function PlayerControlFileName(const BaseName: string): string;
  begin
    Result := ProgramDataPath + 'data' + PathDelim +
      'player_controls' + PathDelim + BaseName;
  end;

  function LoadPlayerControlToDisplayList(const BaseName: string): TGLuint;
  begin
    Result := LoadImageToDisplayList(
      PlayerControlFileName(BaseName), [TRGBAlphaImage], [], 0, 0);
  end;

const
  { Note: this constant must be synchronized with
    NotificationsManager.MaxMessagesCount }
  DarkAreaHeight = 80;

  DarkAreaFadeHeight = 20;
  DarkAreaAlpha = 0.3;
var
  I: Integer;
begin
  { Calculate GLList_NotificationsBackground }
  GLList_NotificationsBackground := glGenListsCheck(1, 'CastlePlay.WindowOpen');
  glNewList(GLList_NotificationsBackground, GL_COMPILE);
  try
    glLoadIdentity;
    glRasterPos2i(0, 0);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
      glColor4f(0, 0, 0, DarkAreaAlpha);
      glRecti(0, 0, Window.Width, DarkAreaHeight);
      for I := 0 to DarkAreaFadeHeight - 1 do
      begin
        glColor4f(0, 0, 0,
          DarkAreaAlpha * (DarkAreaFadeHeight - 1 - I) / DarkAreaFadeHeight);
        glRecti(0, DarkAreaHeight + I, Window.Width, DarkAreaHeight + I + 1);
      end;
    glDisable(GL_BLEND);
  finally glEndList end;

  GLList_InventorySlot := LoadPlayerControlToDisplayList('item_slot.png');

  Font_BFNT_BitstreamVeraSans_m10 := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans_m10);
  Font_BFNT_BitstreamVeraSans     := TGLBitmapFont.Create(@BFNT_BitstreamVeraSans);
end;

procedure WindowClose(Window: TCastleWindowBase);
begin
  FreeAndNil(Font_BFNT_BitstreamVeraSans);
  FreeAndNil(Font_BFNT_BitstreamVeraSans_m10);

  glFreeDisplayList(GLList_NotificationsBackground);
  glFreeDisplayList(GLList_InventorySlot);
end;

initialization
  ShowDebugInfo := false;
  Window.OnOpenList.Add(@WindowOpen);
  Window.OnCloseList.Add(@WindowClose);
end.