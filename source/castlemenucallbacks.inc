procedure EventDown(MouseEvent: boolean; Key: TKey;
  AMouseButton: TMouseButton);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(MouseEvent, Key, #0, AMouseButton) then
    SaveScreen;
end;

procedure KeyDown(glwin: TGLWindow; key: TKey; c: char);
begin
  CurrentMenu.KeyDown(Key, C);
  EventDown(false, Key, mbLeft);

  case C of
    CharEscape: UserQuit := true;
  end;
end;

procedure MouseMove(Glwin: TGLWindow; NewX, NewY: Integer);
begin
  CurrentMenu.MouseMove(NewX, Glwin.Height - NewY,
    Glwin.MousePressed);
end;

procedure MouseDown(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseDown(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
  EventDown(true, K_None, Button);
end;

procedure MouseUp(Glwin: TGLWindow; Button: TMouseButton);
begin
  CurrentMenu.MouseUp(Glwin.MouseX, Glwin.Height - Glwin.MouseY, Button,
    Glwin.MousePressed);
end;

procedure Idle(Glwin: TGLWindow);
begin
  CurrentMenu.Idle(Glwin.IdleSpeed);
  TimeMessagesIdle;
end;

procedure CloseQuery(Glwin: TGLWindow);
begin
  GameCancel(true);
end;
