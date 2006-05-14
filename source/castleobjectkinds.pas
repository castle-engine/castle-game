{
  Copyright 2006 Michalis Kamburelis.

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
}

{ }
unit CastleObjectKinds;

interface

uses Classes, KambiXMLCfg;

const
  DefaultTransparent = false;

type
  { This is a common class for item kind and creature kind. }
  TObjectKind = class
  private
    FTransparent: boolean;
    FVRMLNodeName: string;
    FPrepareRenderDone: boolean;
  protected
    { This should release everything done by PrepareRender.

      When such thing should be called ? E.g. because PrepareRender
      must be done once again, because some attributes (e.g. things
      set by AnimationAttributesSet) changed.

      In this class this just sets PrepareRenderDone to @false,
      and takes care of clearing FirstRootNodesPool. }
    procedure FreePrepareRender; virtual;

    { Use this in PrepareRender to share RootNodes[0]
      of your animations in subclasses. In our destructor and FreePrepareRender
      we will free and clear objects on this list. }
    FirstRootNodesPool: TStringList;
  public
    constructor Create(const AVRMLNodeName: string);
    destructor Destroy; override;

    { Prepare anything needed when starting new game.
      It can call Progress.Step PrepareRenderSteps times. }
    procedure PrepareRender; virtual;

    function PrepareRenderSteps: Cardinal; virtual;

    { Are we between PrepareRender and FreePrepareRender. }
    property PrepareRenderDone: boolean read FPrepareRenderDone;

    { Free any association with current OpenGL context. }
    procedure CloseGL; virtual;

    { This will be used to refer to this kind from VRML models
      (or some other places too).

      This should be a valid VRML node name.
      Also, this mustn't contain '_' or '0' ... '9' chars (we may use them
      internally to encode other info in the same VRML node name)
      --- so actually this should only contain letters, 'a'..'z' and 'A'..'Z'.
      Make this in 'CamelCase' to be consistent. }
    property VRMLNodeName: string read FVRMLNodeName;

    { Should the creature be rendered as transparent or opaque ?
      Each item/creature should either use only partially-transparent
      materials or only fully opaque materials.

      When you leave Transparent = @false while the creature uses
      transparent materials, then the transparent parts of the
      creatures may be completely covered (hidden) by non-transparent objects
      (non-transparent parts of level, other non-transparent creatures
      and items). This depends on rendering order of creatures/items,
      which depends on the order of creatures/items in Level.Creatures
      and Level.Items lists, so no, you can't depend on this artifact
      --- in some situations it will happen, in others not.

      When you leave Transparent = @true while the creature does not use
      transparent materials, then the same bad artifact will happen,
      but the other way around: this time non-transparent parts of
      *this* item/creature will hide *other* transparent items/creatures
      or transparent level parts, even when they shouldn't hide them
      (because they are after them).

      For more reasoning, see CastlePlay.Draw routine. }
    property Transparent: boolean
      read FTransparent write FTransparent default DefaultTransparent;

    procedure LoadFromFile(KindsConfig: TKamXMLConfig); virtual;

    { This is a debug command, will cause FreePrepareRender
      and then (wrapped within Progress.Init...Fini) will
      call PrepareRender. This should reload / regenerate all
      things prepared in PrepareRender. }
    procedure RedoPrepareRender;
  end;

implementation

uses SysUtils, ProgressUnit;

constructor TObjectKind.Create(const AVRMLNodeName: string);
begin
  inherited Create;
  FVRMLNodeName := AVRMLNodeName;
  FTransparent := DefaultTransparent;
  FirstRootNodesPool := TStringList.Create;
end;

destructor TObjectKind.Destroy;
var
  I: Integer;
begin
  for I := 0 to FirstRootNodesPool.Count - 1 do
  begin
    FirstRootNodesPool.Objects[I].Free;
    FirstRootNodesPool.Objects[I] := nil;
  end;
  FreeAndNil(FirstRootNodesPool);

  inherited;
end;

procedure TObjectKind.PrepareRender;
begin
  FPrepareRenderDone := true;
end;

function TObjectKind.PrepareRenderSteps: Cardinal;
begin
  Result := 0;
end;

procedure TObjectKind.FreePrepareRender;
var
  I: Integer;
begin
  for I := 0 to FirstRootNodesPool.Count - 1 do
  begin
    FirstRootNodesPool.Objects[I].Free;
    FirstRootNodesPool.Objects[I] := nil;
  end;
  FirstRootNodesPool.Clear;

  FPrepareRenderDone := false;
end;

procedure TObjectKind.CloseGL;
begin
  { Nothing to do in this class. }
end;

procedure TObjectKind.LoadFromFile(KindsConfig: TKamXMLConfig);
begin
  Transparent := KindsConfig.GetValue(VRMLNodeName + '/transparent',
    DefaultTransparent);
end;

procedure TObjectKind.RedoPrepareRender;
begin
  Progress.Init(PrepareRenderSteps, 'Loading object ' + VRMLNodeName);
  try
    { It's important to do FreePrepareRender after Progress.Init.
      Why ? Because Progress.Init does TGLWindow.SaveScreeToDispList,
      and this may call Glw.OnDraw, and this may want to redraw
      the object (e.g. if creature of given kind already exists
      on the screen) and this requires PrepareRender to be already done.

      So we should call Progress.Init before we invalidate PrepareRender
      work. }
    FreePrepareRender;

    PrepareRender;
  finally Progress.Fini; end;
end;

end.