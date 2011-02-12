{
  Copyright 2006-2011 Michalis Kamburelis.

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

{ }
unit CastleNotifications;

interface

uses GLNotifications, Classes;

var
  { These are all messages passed to @link(Notification).
    Created / destroyed in this unit's initialization / finalization.
    They are not broken (to fit into some particular line width).

    You should clear this when new game starts (use NotificationsClear).

    You cannot modify it directly, you can change it
    only by calling @link(Notification). }
  NotificationsList: TStringList;

{ Add message to NotificationsList and Notifications. }
procedure Notification(const S: string);

{ This must be called within 2d projection. }
procedure NotificationsDraw;

function NotificationsDrawNeeded: boolean;

procedure NotificationsClear;

procedure NotificationsIdle;

implementation

uses SysUtils, GLWindow, CastleWindow, KambiClassUtils;

var
  Notifications: TGLNotifications;

procedure Notification(const S: string);
begin
  if Notifications <> nil then
    Notifications.Show(S);
  NotificationsList.Insert(0, S);
end;

procedure NotificationsDraw;
begin
  Notifications.Draw2D(Window.Width, Window.Height,
                       Window.Width, Window.Height);
end;

function NotificationsDrawNeeded: boolean;
begin
  Result := Notifications.DrawNeeded;
end;

procedure NotificationsClear;
begin
  NotificationsList.Clear;
  Notifications.Clear;
end;

procedure NotificationsIdle;
begin
  Notifications.Idle;
end;

{ initialization / finalization ---------------------------------------------- }

procedure GLWindowOpen(Window: TGLWindow);
begin
  Notifications := TGLNotifications.Create(Window, hpMiddle, vpDown, Window.Width);
  Notifications.MaxMessages := 4;
end;

procedure GLWindowClose(Window: TGLWindow);
begin
  FreeAndNil(Notifications);
end;

initialization
  NotificationsList := TStringList.Create;

  Window.OnOpenList.Add(@GLWindowOpen);
  Window.OnCloseList.Add(@GLWindowClose);
finalization
  FreeAndNil(NotificationsList);
end.
