{
  Copyright 2025 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Dialogs that don't block the main loop, and deferred calls.

  We cannot use the CastleMessages unit (MessageOK, MessageYesNo...),
  and we cannot wait for anything in a "repeat Application.ProcessMessage
  until SomethingHappened" loop. Such loops just hang on the web, where the
  browser must control the main loop.
  See https://castle-engine.io/web , section "Known problems".

  So instead we show a TViewDialog descendant (from CastleDialogViews)
  and get the user answer later, in a callback. }
unit GameDialogs;

interface

uses Classes,
  CastleKeysMouse;

type
  { Answer to a yes/no question. }
  TAnswerBooleanEvent = procedure (const Answer: Boolean) of object;
  { Key / mouse button / mouse wheel pressed by the user. }
  TAnswerPressEvent = procedure (const Answer: TInputPressRelease) of object;
  { Number input by the user. }
  TAnswerCardinalEvent = procedure (const Answer: Cardinal) of object;
  { Parameterless procedure (not a method), for CallDeferred. }
  TDeferredProcedure = procedure;

{ Show a message with an "OK" button.
  Returns immediately, the message is shown on top of the current view.
  @groupBegin }
procedure DialogOK(const S: String); overload;
procedure DialogOK(const SList: TStrings); overload;
{ @groupEnd }

{ Ask a yes/no question.
  Returns immediately, AnswerEvent is called once the user answers. }
procedure DialogYesNo(const S: String; const AnswerEvent: TAnswerBooleanEvent);

{ Ask the user to press a key / mouse button / mouse wheel.
  Returns immediately, AnswerEvent is called once the user presses something. }
procedure DialogPressEvent(const S: String; const AnswerEvent: TAnswerPressEvent);

{ Ask the user for a number.
  Returns immediately, AnswerEvent is called once the user accepts a value.
  AnswerEvent is not called at all if the user cancels. }
procedure DialogCardinal(const S: String; const InitialValue: Cardinal;
  const AnswerEvent: TAnswerCardinalEvent);

{ Call given procedure at the beginning of one of the next frames,
  when we are outside of any event processing.

  This is the moment when it is safe to change the views stack (push / pop
  views, which then immediately execute view Start / Stop / Resume / Pause)
  and to free the things that the stopped views still use.
  Doing this from inside a view Update or Press would only @italic(queue)
  the views stack change (see TCastleContainer.PushView implementation),
  so e.g. freeing something that TCastleView.Stop still needs would crash. }
procedure CallDeferred(const P: TDeferredProcedure);

implementation

uses SysUtils,
  CastleApplicationProperties, CastleDialogViews, CastleLog, CastleUIControls,
  GameWindow;

type
  { Owner of the ApplicationProperties.OnUpdate callback, which is where
    we react to the answered dialogs and run the deferred procedures. }
  TDialogsHandler = class
    procedure Update(Sender: TObject);
  end;

var
  Handler: TDialogsHandler;

  { Dialog shown now, waiting for the user answer. @nil if none. }
  CurrentDialog: TViewDialog;
  { Callback for CurrentDialog. Only one of them is ever assigned. }
  CurrentAnswerBoolean: TAnswerBooleanEvent;
  CurrentAnswerPress: TAnswerPressEvent;
  CurrentAnswerCardinal: TAnswerCardinalEvent;

  DeferredProcedures: array of TDeferredProcedure;

procedure ShowDialog(const Dialog: TViewDialog);
begin
  { Only one dialog at a time is enough for this game.
    Note that showing a new dialog from the answer callback of the previous
    dialog works OK, as CurrentDialog is already cleared then. }
  if CurrentDialog <> nil then
  begin
    WritelnWarning('Showing a dialog while the previous one is not answered yet');
    Window.Container.PopView(CurrentDialog);
    FreeAndNil(CurrentDialog);
  end;

  CurrentAnswerBoolean := nil;
  CurrentAnswerPress := nil;
  CurrentAnswerCardinal := nil;

  { Note: we deliberately leave Dialog.BackgroundScreenshot = false,
    unlike CastleMessages did. The view underneath stays renderable (it is only
    paused), so we can just render it, and we avoid the cost of grabbing
    the screen contents on every dialog. }

  CurrentDialog := Dialog;
  Window.Container.PushView(Dialog);
end;

procedure DialogOK(const S: String);
var
  Dialog: TViewDialogOK;
begin
  Dialog := TViewDialogOK.Create(nil);
  Dialog.Caption := S;
  ShowDialog(Dialog);
end;

procedure DialogOK(const SList: TStrings);
var
  Dialog: TViewDialogOK;
begin
  Dialog := TViewDialogOK.Create(nil);
  Dialog.Text.Assign(SList);
  ShowDialog(Dialog);
end;

procedure DialogYesNo(const S: String; const AnswerEvent: TAnswerBooleanEvent);
var
  Dialog: TViewDialogYesNo;
begin
  Dialog := TViewDialogYesNo.Create(nil);
  Dialog.Caption := S;
  ShowDialog(Dialog);
  CurrentAnswerBoolean := AnswerEvent;
end;

procedure DialogPressEvent(const S: String; const AnswerEvent: TAnswerPressEvent);
var
  Dialog: TViewDialogPressEvent;
begin
  Dialog := TViewDialogPressEvent.Create(nil);
  Dialog.Caption := S;
  ShowDialog(Dialog);
  CurrentAnswerPress := AnswerEvent;
end;

procedure DialogCardinal(const S: String; const InitialValue: Cardinal;
  const AnswerEvent: TAnswerCardinalEvent);
var
  Dialog: TViewDialogInput;
begin
  Dialog := TViewDialogInput.Create(nil);
  Dialog.Caption := S;
  Dialog.Answer := IntToStr(InitialValue);
  Dialog.AllowedChars := ['0'..'9'];
  Dialog.MinLength := 1;
  Dialog.CanCancel := true;
  ShowDialog(Dialog);
  CurrentAnswerCardinal := AnswerEvent;
end;

procedure CallDeferred(const P: TDeferredProcedure);
begin
  SetLength(DeferredProcedures, Length(DeferredProcedures) + 1);
  DeferredProcedures[High(DeferredProcedures)] := P;
end;

{ TDialogsHandler ------------------------------------------------------------ }

procedure TDialogsHandler.Update(Sender: TObject);

  procedure HandleAnsweredDialog;
  var
    Dialog: TViewDialog;
    EventBoolean: TAnswerBooleanEvent;
    EventPress: TAnswerPressEvent;
    EventCardinal: TAnswerCardinalEvent;
    AnswerBoolean: Boolean;
    AnswerPress: TInputPressRelease;
    AnswerCardinal: Integer;
    AnswerCardinalValid: Boolean;
  begin
    Dialog := CurrentDialog;
    CurrentDialog := nil;

    EventBoolean := CurrentAnswerBoolean;
    EventPress := CurrentAnswerPress;
    EventCardinal := CurrentAnswerCardinal;
    CurrentAnswerBoolean := nil;
    CurrentAnswerPress := nil;
    CurrentAnswerCardinal := nil;

    AnswerBoolean := false;
    AnswerCardinal := 0;
    AnswerCardinalValid := false;

    if Dialog is TViewDialogYesNo then
      AnswerBoolean := TViewDialogYesNo(Dialog).Answer
    else
    if Dialog is TViewDialogPressEvent then
      AnswerPress := TViewDialogPressEvent(Dialog).Answer
    else
    if Dialog is TViewDialogInput then
      AnswerCardinalValid :=
        (not TViewDialogInput(Dialog).AnswerCancelled) and
        TryStrToInt(TViewDialogInput(Dialog).Answer, AnswerCardinal) and
        (AnswerCardinal >= 0);

    { Normally the dialog already popped itself (PopOnAnswered),
      but be safe: never free a view that is still on the views stack. }
    if Window.Container.FrontView = Dialog then
      Window.Container.PopView(Dialog);

    { Free the dialog before calling the answer event,
      as the answer event may show a new dialog. }
    FreeAndNil(Dialog);

    if Assigned(EventBoolean) then
      EventBoolean(AnswerBoolean);
    if Assigned(EventPress) then
      EventPress(AnswerPress);
    if Assigned(EventCardinal) and AnswerCardinalValid then
      EventCardinal(AnswerCardinal);
  end;

  procedure RunDeferredProcedures;
  var
    RunNow: array of TDeferredProcedure;
    I: Integer;
  begin
    { Take the list aside, as the called procedures may add new ones. }
    RunNow := DeferredProcedures;
    DeferredProcedures := nil;
    for I := 0 to High(RunNow) do
      RunNow[I]();
  end;

begin
  if (CurrentDialog <> nil) and CurrentDialog.Answered then
    HandleAnsweredDialog;
  if Length(DeferredProcedures) <> 0 then
    RunDeferredProcedures;
end;

initialization
  Handler := TDialogsHandler.Create;
  ApplicationProperties.OnUpdate.Add(@Handler.Update);
finalization
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove(@Handler.Update);
  FreeAndNil(Handler);
  FreeAndNil(CurrentDialog);
end.
