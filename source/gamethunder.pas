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

{ Small unit to do "thunder" effect as seen in "cages" level.
  Based on my "lets_take_a_walk" unit thunder.pas. }
unit GameThunder;

interface

uses X3DNodes;

type
  { Rendering and making sound of thunder effect.

    Note that many methods (but not RenderLight)
    use Level.AnimationTime for timing. }
  TThunderEffect = class
  private
    LastBeginTime, NextBeginTime: Single;
    ThunderLightNode: TDirectionalLightNode;
    ThunderLight: TLightInstance;
  public
    constructor Create;
    destructor Destroy; override;

    { Add thunder light, if visible. }
    procedure AddLight(const BaseLights: TLightInstancesList);

    procedure Idle;
    { Force thunder happening in next Idle call. }
    procedure ForceNow;
  end;

implementation

uses SysUtils, GL, GLU, CastleGLUtils, VectorMath, GameSound, GamePlay, Math;

constructor TThunderEffect.Create;
begin
  inherited;
  ThunderLightNode := TDirectionalLightNode.Create('', '');
  ThunderLightNode.FdDirection.Value := Vector3Single(0, -1, 1);
  ThunderLightNode.FdAmbientIntensity.Value := 0.5;
  ThunderLightNode.FdColor.Value := Vector3Single(0.5, 0.5, 1);

  ThunderLight.Node := ThunderLightNode;
  ThunderLight.Transform := IdentityMatrix4Single;
  ThunderLight.TransformScale := 1;
  ThunderLight.Location := ZeroVector3Single;
  ThunderLight.Direction := ThunderLightNode.FdDirection.Value;
  ThunderLight.Radius := MaxSingle;
  ThunderLight.WorldCoordinates := true;
end;

destructor TThunderEffect.Destroy;
begin
  FreeAndNil(ThunderLightNode);
  inherited;
end;

procedure TThunderEffect.AddLight(const BaseLights: TLightInstancesList);

  function Visible: boolean;
  var
    ThunderTime: Single;
  begin
    Result := false;
    if LastBeginTime <> 0 then
    begin
      ThunderTime := Level.AnimationTime - LastBeginTime;
      if (ThunderTime < 1.0) or
         ((1.5 < ThunderTime) and (ThunderTime < 2.5)) then
        Result := true;
    end;
  end;

begin
  if Visible then
    BaseLights.Add(ThunderLight);
end;

procedure TThunderEffect.Idle;
begin
  if NextBeginTime = 0 then
    NextBeginTime := Level.AnimationTime + 10 + Random(10);

  if NextBeginTime <= Level.AnimationTime then
  begin
    LastBeginTime := Level.AnimationTime;
    NextBeginTime := Level.AnimationTime + 10 + Random(20);

    {ThunderAllocatedSound := }SoundEngine.Sound(stThunder);
  end;
end;

procedure TThunderEffect.ForceNow;
begin
  NextBeginTime := Level.AnimationTime;
end;

end.