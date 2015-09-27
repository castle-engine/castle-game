procedure Press(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(CharEscape) then UserQuit := true;
end;

procedure CloseQuery(Container: TUIContainer);
begin
  GameCancel(true);
end;
