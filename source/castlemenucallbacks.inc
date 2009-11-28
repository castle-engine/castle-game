procedure EventDown(MouseEvent: boolean; Key: TKey;
  AMouseButton: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  EventDown(false, Key, mbLeft);

  case C of
    CharEscape: UserQuit := true;
  end;
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  EventDown(true, K_None, Button);
end;

procedure Idle(Glwin: TGLWindow);
begin
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;
