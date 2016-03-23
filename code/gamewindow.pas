{
  Copyright 2006-2014 Michalis Kamburelis.

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

{ Global @link(Window) variable. }
unit GameWindow;

interface

uses Classes,
  CastleVectors, CastleWindow, CastleUIControls, CastleKeysMouse,
  CastleRectangles, CastleControls, CastleCameras;

type
  TTouchInterface = (
    tiNone,
    tiCtlWalkCtlRotate,
    tiCtlWalkDragRotate,
    tiDragRotate,
    tiCtlFlyCtlWalkDragRotate,
    tiCtlPanXYDragRotate);

  TGameWindow = class(TCastleWindowCustom)
  private
    FControl: array [boolean { right side? }] of TCastleTouchControl;
    FTouchInterface: TTouchInterface;
  protected
    procedure DoUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;

    { Configure touch controls to be displayed on the window.
      This automatically manages under the hood 0, 1 or 2
      TCastleTouchControl instances, placing them at suitable positions
      and handling their operations. }
    property TouchInterface: TTouchInterface read FTouchInterface;
    procedure SetTouchInterface(const Value: TTouchInterface; const Camera: TCamera);
  end;

  TGlobalCatchInput = class(TUIControl)
    function Press(const Event: TInputPressRelease): boolean; override;
    function CapturesEventsAtPosition(const Position: TVector2Single): boolean; override;
  end;

var
  { @noAutoLinkHere }
  Window: TGameWindow;

  { This way our "save screen" button works in game, all menus, credits
    and such. Make sure this is always on Window.Controls list. }
  GlobalCatchInput: TGlobalCatchInput;

implementation

uses SysUtils,
  CastleInputs, CastleGameNotifications, CastleFilesUtils, CastleSoundEngine,
  CastleUtils,
  GameSound, GameInputs;

{ TGlobalCatchInput ---------------------------------------------------------- }

function TGlobalCatchInput.Press(const Event: TInputPressRelease): boolean;

  { Saves a screen, causing also appropriate Notification and sound. }
  procedure AutoSaveScreen;
  var
    URL: string;
  begin
    URL := FileNameAutoInc(ApplicationName + '_screen_%d.png');
    Window.SaveScreen(URL);
    Notifications.Show('Screen saved to ' + URL);
    SoundEngine.Sound(stSaveScreen);
  end;

begin
  Result := inherited;
  if Result then Exit;

  if Input_SaveScreen.IsEvent(Event) then
  begin
    AutoSaveScreen;
    Result := true;
  end;
end;

function TGlobalCatchInput.CapturesEventsAtPosition(
  const Position: TVector2Single): boolean;
begin
  Result := true; // always catch input
end;

{ TGameWindow ---------------------------------------------------------------- }

constructor TGameWindow.Create(AOwner: TComponent);
begin
  inherited;
  GlobalCatchInput := TGlobalCatchInput.Create(Self);
  Controls.InsertBack(GlobalCatchInput);
  Controls.InsertFront(Notifications);
  Notifications.KeepInFront := true;
end;

procedure TGameWindow.DoUpdate;
var
  Tx, Ty, Tz, TLength, Rx, Ry, Rz, RAngle: Double;
  RightSide: boolean;
begin
  inherited;

  if (FControl[false] <> nil) or
     (FControl[true] <> nil) then
  begin
    Tx := 0; Ty := 0; Tz := 0; TLength := 0;
    Rx := 0; Ry := 0; Rz := 0; RAngle := 0;

    for RightSide in boolean do
      if FControl[RightSide] <> nil then
      begin
        FControl[RightSide].GetSensorTranslation(Tx, Ty, Tz, TLength);
        FControl[RightSide].GetSensorRotation(Rx, Ry, Rz, RAngle);
      end;

    { send to all 2D controls, including viewports }
    Container.EventSensorTranslation(Tx, Ty, Tz, TLength, Fps.UpdateSecondsPassed);
    Container.EventSensorRotation(Rx, Ry, Rz, RAngle, Fps.UpdateSecondsPassed);
  end;
end;

procedure TGameWindow.SetTouchInterface(const Value: TTouchInterface; const Camera: TCamera);

  procedure UpdateTouchController(
    const RightSide, CtlVisible: boolean; const Mode: TCastleTouchCtlMode);
  var
    NewControl: TCastleTouchControl;
  begin
    if FControl[RightSide] <> nil then
    begin
      if CtlVisible then
        FControl[RightSide].TouchMode := Mode else
        FreeAndNil(FControl[RightSide]); // this automatically removes FControl[RightSide] from Controls list
    end else
    if CtlVisible then
    begin
      NewControl := TCastleTouchControl.Create(self);
      NewControl.TouchMode := Mode;
      NewControl.Scale := 2; // TODO: hardcoded
      if not RightSide then
        NewControl.Position := tpLeft else
        NewControl.Position := tpRight;
      Controls.InsertFront(NewControl);
      FControl[RightSide] := NewControl;
    end;
  end;

var
  WalkCamera: TWalkCamera;

  procedure UpdateTouchControllers(
    const MouseDragMode: TMouseDragMode;
    const LeftVisible, RightVisible: boolean;
    const LeftMode: TCastleTouchCtlMode = ctcmWalking;
    const RightMode: TCastleTouchCtlMode = ctcmWalking);
  begin
    UpdateTouchController(false, LeftVisible , LeftMode);
    UpdateTouchController(true , RightVisible, RightMode);
    if WalkCamera <> nil then
      WalkCamera.MouseDragMode := MouseDragMode;
  end;

begin
  if FTouchInterface <> Value then
  begin
    FTouchInterface := Value;

    WalkCamera := nil;
    if Camera <> nil then
    begin
      if Camera is TUniversalCamera then
        WalkCamera := (Camera as TUniversalCamera).Walk else
      if Camera is TWalkCamera then
        WalkCamera := Camera as TWalkCamera;
    end;

    case Value of
      tiNone:
        UpdateTouchControllers(mdWalk, false, false);
      tiCtlWalkCtlRotate:
        UpdateTouchControllers(mdNone, true, true, ctcmWalking, ctcmHeadRotation);
      tiCtlWalkDragRotate:
        UpdateTouchControllers(mdRotate, false, true, ctcmWalking, ctcmWalking);
      tiDragRotate:
        UpdateTouchControllers(mdRotate, false, false, ctcmWalking, ctcmWalking);
      tiCtlFlyCtlWalkDragRotate:
        UpdateTouchControllers(mdRotate, true, true, ctcmFlyUpdown, ctcmWalking);
      tiCtlPanXYDragRotate:
        UpdateTouchControllers(mdRotate, false, true, ctcmPanXY, ctcmPanXY);
      else raise EInternalError.Create('Value unhandled in SetTouchInterface');
    end;
  end;
end;

initialization
  Window := TGameWindow.Create(nil);
  Window.RenderStyle := rs3D;
finalization
  FreeAndNil(Window);
end.
