procedure EventDown(AKey: TKey;
  AMousePress: boolean; AMouseButton: TMouseButton;
  AMouseWheel: TMouseWheelDirection);
begin
  if CastleInput_SaveScreen.Shortcut.IsEvent(AKey, #0,
    AMousePress, AMouseButton, AMouseWheel) then
    SaveScreen;
end;

procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  EventDown(Key, false, mbLeft, mwNone);

  case C of
    CharEscape: UserQuit := true;
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
