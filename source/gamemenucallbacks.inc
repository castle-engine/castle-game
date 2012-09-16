procedure Press(Window: TCastleWindowBase; const Event: TInputPressRelease);
begin
  if Event.IsKey(CharEscape) then UserQuit := true;
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  GameCancel(true);
end;
