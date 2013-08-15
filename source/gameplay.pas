{
  Copyright 2006-2013 Michalis Kamburelis.

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

uses Classes, CastleLevels, CastlePlayer, Castle3D;

{ Play the game.
  SceneManager and Player global variables must be already initialized.
  If PrepareNewPlayer then it will call SceneManager.Logic.PrepareNewPlayer
  right before starting the actual game. }
procedure PlayGame(PrepareNewPlayer: boolean);

type
  TCastle1SceneManager = class(TGameSceneManager)
  protected
    function PointingDeviceActivate3D(const Item: T3D; const Active: boolean;
      const Distance: Single): boolean; override;
  end;

var
  { Currently used player by PlayGame. nil if PlayGame doesn't work
    right now.
    @noAutoLinkHere }
  Player: TPlayer;

  { Currently used scene manager by PlayGame. nil if PlayGame doesn't work
    right now.
    @noAutoLinkHere }
  SceneManager: TCastle1SceneManager;

{ Load new level or (if NextLevelName is empty) end the game.

  Loading of new level doesn't happen immediately, as it could interfere
  with various other operations. Instead, this procedure merely records
  the need to do it, and actual work will be done later at good code place. }
procedure LevelFinished(NextLevelName: string);

{ If some LevelFinished call is scheduled, this will force changing
  level @italic(now). Don't use this, unless you know that you can
  safely change the level now (which means that old level will be
  destroyed, along with all it's items, creatures etc. references). }
procedure LevelFinishedFlush;

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

uses SysUtils, CastleUtils, CastleWindow, GameInputs,
  CastleWindowModes, GL, GLU, GLExt, CastleGLUtils, CastleMessages, GameWindow,
  CastleVectors, CastleImages, Math, GameHelp, CastleUIControls, CastleSoundEngine,
  GameItems, CastleStringUtils, CastleCreatures, CastleItems,
  CastleFilesUtils, CastleInputs, GameGameMenu, GameDebugMenu, GameSound,
  GameVideoOptions, CastleColors, CastleSceneManager,
  CastleGameNotifications, GameControlsMenu, CastleControls,
  GameLevelSpecific, CastleTimeUtils, CastleGLImages, CastleKeysMouse;

var
  GLList_NotificationsBackground: TGLuint;
  GLInventorySlot: TGLImage;
  GLBlankIndicatorImage: TGLImage;
  GLRedIndicatorImage: TGLImage;
  GLBlueIndicatorImage: TGLImage;
  GLBossIndicatorImage: TGLImage;

  DisplayFpsUpdateTick: TMilisecTime;
  DisplayFpsFrameTime: Single;
  DisplayFpsRealTime: Single;

  ShowDebugInfo: boolean;

  LevelFinishedSchedule: boolean = false;
  { If LevelFinishedSchedule, then this is not-'', and should be the name
    of next Level to load. }
  LevelFinishedNextLevelName: string;

  GameControls: TUIControlList;

{ TGame2DControls ------------------------------------------------------------ }

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
    InventorySlotsVisibleInColumn := ContainerHeight div InventorySlotHeight;

    { Draw at least InventorySlotsVisibleInColumn slots,
      possibly drawing empty slots. This is needed, because
      otherwise when no items are owned player doesn't see any
      effect of changing InventoryVisible. }
    for I := 0 to Max(Player.Inventory.Count - 1,
      InventorySlotsVisibleInColumn - 1) do
    begin
      X := ItemSlotX(I);
      Y := ItemSlotY(I);

      GLInventorySlot.Alpha := acFullRange;
      GLInventorySlot.Draw(X, Y);
    end;

    for I := 0 to Player.Inventory.Count - 1 do
    begin
      X := ItemSlotX(I);
      Y := ItemSlotY(I);

      Player.Inventory[I].Resource.GLImage.Alpha := acSimpleYesNo;
      Player.Inventory[I].Resource.GLImage.Draw(
        X + InventorySlotMargin, Y + InventorySlotMargin);
    end;

    if Between(Player.InventoryCurrentItem, 0, Player.Inventory.Count - 1) then
    begin
      GLRectangleBorder(
        ItemSlotX(Player.InventoryCurrentItem) + InventorySlotMargin,
        ItemSlotY(Player.InventoryCurrentItem) + InventorySlotMargin,
        ItemSlotX(Player.InventoryCurrentItem)
          + InventorySlotWidth - InventorySlotMargin,
        ItemSlotY(Player.InventoryCurrentItem)
          + InventorySlotHeight - InventorySlotMargin,
        Vector4Single(0.8, 0.8, 0.8, 1));
    end;

    glColor4f(1, 1, 0.5, 1);
    for I := 0 to Player.Inventory.Count - 1 do
    begin
      X := ItemSlotX(I);
      Y := ItemSlotY(I);

      S := Player.Inventory[I].Resource.Caption;
      if Player.Inventory[I].Quantity <> 1 then
        S += ' (' + IntToStr(Player.Inventory[I].Quantity) + ')';
      UIFontSmall.Print(X + InventorySlotMargin, Y + InventorySlotMargin, S);
    end;
  end;

  const
    { line number 1 is for "flying" text in Player.Draw2D }
    LineDeadOrWinner = 2;
    LinePressEscape = 3;
    LinePressAttack = 4;
    LineFPS = 5;
    LineShadowVolumesCounts = 6;

  procedure PosLine(const Line: Cardinal);
  begin
    SetWindowPos(0, ContainerHeight - UIFont.RowHeight * Line - 10 { margin });
  end;

  procedure DoShowFPS;
  begin
    glColorv(Vector3Single(0.7, 0.7, 0.7));
    PosLine(LineFPS);

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

    UIFont.PrintAndMove(Format('FPS : %f (real : %f). Shapes : %d / %d',
      [DisplayFpsFrameTime, DisplayFpsRealTime,
       SceneManager.Statistics.ShapesRendered, SceneManager.Statistics.ShapesVisible]));
  end;

  procedure DoShowShadowVolumesCounts;
  begin
    if GLFeatures.ShadowVolumesPossible and ShadowVolumes then
    begin
      glColorv(Vector3Single(0.7, 0.7, 0.7));
      PosLine(LineShadowVolumesCounts);
      UIFont.PrintAndMove(Format('No shadow %d + zpass %d + zfail (no l cap) %d + zfail (l cap) %d = all %d',
        [ SceneManager.ShadowVolumeRenderer.CountShadowsNotVisible,
          SceneManager.ShadowVolumeRenderer.CountZPass,
          SceneManager.ShadowVolumeRenderer.CountZFailNoLightCap,
          SceneManager.ShadowVolumeRenderer.CountZFailAndLightCap,
          SceneManager.ShadowVolumeRenderer.CountScenes ]));
    end;
  end;

  procedure DoShowDeadOrFinishedKeys;

    const
      SPressEscapeToExit = 'Press [Escape] to exit to menu.';

    function SPressAttackToRestart: string;
    begin
      Result := 'Press [Interact] (' +
        Input_Interact.Description('not assigned') +
        ') to restart the level.';
    end;

  begin
    PosLine(LinePressEscape);
    UIFont.PrintAndMove(SPressEscapeToExit);
    PosLine(LinePressAttack);
    UIFont.PrintAndMove(SPressAttackToRestart);
  end;

  procedure DoShowDeadInfo;
  begin
    glColorv(Vector3Single(1, 0, 0));
    PosLine(LineDeadOrWinner);
    UIFont.PrintAndMove('You''re dead.');
    DoShowDeadOrFinishedKeys;
  end;

  procedure DoShowGameWinInfo;
  begin
    glColorv(Vector3Single(0.8, 0.8, 0.8));
    PosLine(LineDeadOrWinner);
    UIFont.PrintAndMove('Game finished.');
    DoShowDeadOrFinishedKeys;
  end;

  procedure RenderLifeIndicator(const ALife, AMaxLife: Single;
    const GLFullIndicatorImage: TGLImage;
    const XMove: Integer; const PrintText: boolean);
  const
    IndicatorHeight = 120;
    IndicatorWidth = 40;
    IndicatorMargin = 5;
  var
    LifeMapped: Integer;
    LifeTextPosition: Integer;
    LifeText: string;
  begin
    SetWindowPos(XMove + IndicatorMargin, IndicatorMargin);
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.5);

      LifeMapped := Round(MapRange(ALife, 0, AMaxLife, 0, IndicatorHeight));

      { we control alpha test manually }
      GLFullIndicatorImage.Alpha := acNone;
      GLBlankIndicatorImage.Alpha := acNone;

      { Note that Life may be > MaxLife, and
        Life may be < 0. }
      if LifeMapped >= IndicatorHeight then
        GLFullIndicatorImage.Draw else
      if LifeMapped < 0 then
        GLBlankIndicatorImage.Draw else
      begin
        glEnable(GL_SCISSOR_TEST);
          glScissor(IndicatorMargin, IndicatorMargin, ContainerWidth, LifeMapped);
          GLFullIndicatorImage.Draw;
          glScissor(IndicatorMargin, IndicatorMargin + LifeMapped,
            ContainerWidth, ContainerHeight);
          GLBlankIndicatorImage.Draw;
        glDisable(GL_SCISSOR_TEST);
      end;

    glDisable(GL_ALPHA_TEST);

    if PrintText and (ALife > 0) then
    begin
      glColorv(Vector3Single(0.8, 0.8, 0.8));
      LifeText := Format('%d', [Round(ALife)]);
      LifeTextPosition := XMove + IndicatorMargin +
        (IndicatorWidth - UIFont.TextWidth(LifeText)) div 2;
      MaxTo1st(LifeTextPosition, IndicatorMargin);
      SetWindowPos(LifeTextPosition, IndicatorMargin + IndicatorHeight div 2);
      UIFont.PrintAndMove(LifeText);
    end;
  end;

  procedure PlayerRender2D;
  var
    S: string;
  begin
    RenderLifeIndicator(Player.Life, Player.MaxLife, GLRedIndicatorImage, 0, true);

    if Player.Flying then
    begin
      glColorv(White3Single);
      SetWindowPos(0, ContainerHeight - UIFont.RowHeight - 5 { margin });
      if Player.FlyingTimeOut > 0 then
        S := Format(' (%d more seconds)', [Floor(Player.FlyingTimeOut)]);
      UIFont.PrintAndMove('Flying' + S);
    end;

    glLoadIdentity;
    if Player.Dead then
      GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight, Red3Single, 1.0) else
    begin
      if Player.Swimming = psUnderWater then
        GLBlendRectangle(0, 0, ContainerWidth, ContainerHeight,
          GL_ONE, GL_SRC_ALPHA, Vector4Single(0, 0, 0.1, 0.5));

      { Possibly, Player.FadeOut* will be applied on top of water effect,
        that's Ok --- they'll mix. }
      GLFadeRectangle(0, 0, ContainerWidth, ContainerHeight,
        Player.FadeOutColor, Player.FadeOutIntensity);
    end;
  end;

var
  BossLife: Single;
  BossMaxLife: Single;
begin
  if DebugRenderForLevelScreenshot then Exit;

  if Notifications.DrawStyle <> dsNone then
    glCallList(GLList_NotificationsBackground);

  if Player.InventoryVisible then
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

  if (SceneManager.Logic <> nil) and
     (SceneManager.Logic is TBossLevel) and
     TBossLevel(SceneManager.Logic).BossIndicator(BossLife, BossMaxLife) then
  begin
    RenderLifeIndicator(BossLife, BossMaxLife,
      GLBossIndicatorImage, ContainerWidth - 150, false);
  end;

  PlayerRender2D;
end;

{ TCastle1SceneManager ------------------------------------------------------- }

function TCastle1SceneManager.PointingDeviceActivate3D(const Item: T3D;
  const Active: boolean; const Distance: Single): boolean;
const
  VisibleDistance = 60.0;
var
  S: string;
  I: TItemOnWorld;
  C: TCreature;
begin
  Result := inherited;
  if Result then Exit;

  if Active and
     ( (Item is TItemOnWorld) or
       (Item is TCreature) ) and
     (Distance <= VisibleDistance) then
  begin
    if Item is TCreature then
    begin
      C := TCreature(Item);
      S := Format('You see a creature "%s"', [C.Resource.Name]);

      if C.Life >= C.MaxLife then
        S += ' (not wounded)' else
      if C.Life >= C.MaxLife / 3 then
        S += ' (wounded)' else
      if C.Life > 0 then
        S += ' (very wounded)' else
        S += ' (dead)';
    end else
    begin
      I := TItemOnWorld(Item);
      S := Format('You see an item "%s"', [I.Item.Resource.Caption]);
      if I.Item.Quantity <> 1 then
        S += Format(' (quantity %d)', [I.Item.Quantity]);
    end;

    Notifications.Show(S);
    Result := true;
  end;
end;

procedure Update(Window: TCastleWindowBase);
const
  GameWinPosition1: TVector3Single = (30.11, 146.27, 1.80);
  GameWinPosition2: TVector3Single = (30.11, 166.27, 1.80);
  GameWinDirection: TVector3Single = (0, 1, 0);
  GameWinUp: TVector3Single = (0, 0, 1);
var
  Cages: TCagesLevel;
begin
  LevelFinishedFlush;

  if GameWin and (SceneManager.Logic is TCagesLevel) then
  begin
    Cages := TCagesLevel(SceneManager.Logic);
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
  ImageBackground: TCastleImageControl;
begin
  if LevelFinishedSchedule then
  begin
    LevelFinishedSchedule := false;

    { create a background image when loading new level.
      SceneManager will initialize progress bar when SceneManager.Level
      will be released, so the background will be completely black
      if we don't set something up here. }
    ImageBackground := TCastleImageControl.Create(nil);
    try
      { TODO: nicer image background: blur or such? }
      ImageBackground.Image := Window.SaveScreen;
      Window.Controls.Add(ImageBackground);

      SceneManager.LoadLevel(LevelFinishedNextLevelName);
    finally
      { this will also remove ImageBackground from Window.Controls }
      FreeAndNil(ImageBackground);
    end;
  end;
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

procedure Press(Window: TCastleWindowBase; const Event: TInputPressRelease);

  procedure UseLifePotion;
  var
    UsedItemIndex: Integer;
  begin
    UsedItemIndex := Player.Inventory.FindResource(LifePotion);
    if UsedItemIndex <> -1 then
      Player.UseItem(UsedItemIndex) else
      Notifications.Show('You don''t have any life potion');
  end;

  procedure DoDebugMenu;
  begin
    SceneManager.Paused := true;
    ShowDebugMenu(GameControls);
    SceneManager.Paused := false;
  end;

  procedure RestartLevel;
  begin
    { normal interaction is already handled because
      TCastleSceneManager.Input_Interact is equal to interact key. }
    if GameWin or Player.Dead then
    begin
      GameEndedWantsRestart := SceneManager.Info.Name;
      GameEnded := true;
    end;
  end;

begin
  if Event.IsKey(CharEscape) then
  begin
    if Player.Dead or GameWin then
      GameCancel(false) else
    begin
      SceneManager.Paused := true;
      ShowGameMenu(GameControls);
      PlayerUpdateMouseLook(Player);
      SceneManager.Paused := false;
    end;
  end;

  if (Player <> nil) and not (Player.Blocked or Player.Dead) then
  begin
    if Input_UseLifePotion.IsEvent(Event) then
      UseLifePotion;
  end;

  { Other keys. }
  if Input_ViewMessages.IsEvent(Event) then
    ViewGameMessages else
  if Input_FPSShow.IsEvent(Event) then
    ShowDebugInfo := not ShowDebugInfo else
  if Input_Interact.IsEvent(Event) then
    RestartLevel else
  if Input_DebugMenu.IsEvent(Event) then
    DoDebugMenu;
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
  GameWin := false;

  LevelFinishedSchedule := false;

  SavedMode := TGLMode.CreateReset(Window, 0, true, nil, nil, @CloseQuery);
  try
    Window.AutoRedisplay := true;

    Window.OnUpdate := @Update;
    Window.OnPress := @Press;
    Window.OnDrawStyle := ds3D;

    C2D := TGame2DControls.Create(nil);
    GameControls := TUIControlList.CreateFromArray(false, [Notifications, C2D, SceneManager]);

    Window.Controls.AddList(GameControls);

    GameEnded := false;
    GameEndedWantsRestart := '';

    MessagesTheme.RectColor[3] := 0.4;

    if PrepareNewPlayer then
      SceneManager.Logic.PrepareNewPlayer(Player);

    Notifications.Show('Hint: press "Escape" for game menu');

    repeat
      Application.ProcessMessage(true, true);
    until GameEnded;
  finally
    { Clear some Player.Camera callbacks. }
    SceneManager.OnCameraChanged := nil;

    FreeAndNil(GameControls);
    FreeAndNil(C2D);
    FreeAndNil(SavedMode);
  end;
end;

procedure LevelFinished(NextLevelName: string);
begin
  if NextLevelName = '' then
  begin
    Notifications.Show('Congratulations, game finished');
    GameWin := true;
    Player.Blocked := true;
    SoundEngine.MusicPlayer.Sound := stGameWinMusic;
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

type
  TGamePlay = class
    class function CreatureExists(const Item: T3D): boolean;
    class function ItemOnWorldExists(const Item: T3D): boolean;
  end;

class function TGamePlay.CreatureExists(const Item: T3D): boolean;
begin
  Result := not GameWin;
end;

class function TGamePlay.ItemOnWorldExists(const Item: T3D): boolean;
begin
  Result := (not GameWin) and (not DebugRenderForLevelScreenshot);
end;

{ initialization / finalization ---------------------------------------------- }

procedure WindowOpen(const Container: IUIContainer);

  function PlayerControlURL(const BaseName: string): string;
  begin
    Result := ApplicationData('player_controls/' + BaseName);
  end;

  function LoadPlayerControlToGL(const BaseName: string): TGLImage;
  begin
    Result := TGLImage.Create(PlayerControlURL(BaseName), [TRGBAlphaImage]);
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
  GLList_NotificationsBackground := glGenListsCheck(1, 'CastlePlay.WindowOpen');
  glNewList(GLList_NotificationsBackground, GL_COMPILE);
  try
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

  GLInventorySlot := LoadPlayerControlToGL('item_slot.png');
  GLBlankIndicatorImage := LoadPlayerControlToGL('blank.png');
  GLRedIndicatorImage := LoadPlayerControlToGL('red.png');
  GLBlueIndicatorImage := LoadPlayerControlToGL('blue.png');
  GLBossIndicatorImage := LoadPlayerControlToGL('boss.png');
end;

procedure WindowClose(const Container: IUIContainer);
begin
  glFreeDisplayList(GLList_NotificationsBackground);
  FreeAndNil(GLInventorySlot);
  FreeAndNil(GLBlankIndicatorImage);
  FreeAndNil(GLRedIndicatorImage);
  FreeAndNil(GLBlueIndicatorImage);
  FreeAndNil(GLBossIndicatorImage);
end;

initialization
  ShowDebugInfo := false;
  OnGLContextOpen.Add(@WindowOpen);
  OnGLContextClose.Add(@WindowClose);
  OnCreatureExists := @TGamePlay(nil).CreatureExists;
  OnItemOnWorldExists := @TGamePlay(nil).ItemOnWorldExists;
  T3DOrient.DefaultOrientation := otUpZDirectionX;
end.
