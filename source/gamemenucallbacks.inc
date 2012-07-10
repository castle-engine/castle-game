procedure KeyDown(Window: TCastleWindowBase; key: TKey; c: char);
begin
  case C of
    CharEscape: UserQuit := true;
  end;
end;

procedure CloseQuery(Window: TCastleWindowBase);
begin
  GameCancel(true);
end;
