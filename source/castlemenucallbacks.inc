procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0,
    AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);

  case C of
    CharEscape: UserQuit := true;
  end;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(K_None, true, Button, mwNone);
end;

procedure MouseWheel(Glwin: TGLWindow; const Scroll: Single; const Vertical: boolean);
begin
  EventDown(K_None, false, mbLeft, MouseWheelDirection(Scroll, Vertical));
end;

procedure Idle(Glwin: TGLWindow);
begin
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;
