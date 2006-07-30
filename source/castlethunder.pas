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

{ Small unit to do "thunder" effect as seen in "cages" level.
  Based on my "lets_take_a_walk" unit thunder.pas. }
unit CastleThunder;

interface

type
  { Rendering and making sound of thunder effect.

    Note that many methods (but not InitGLLight)
    use Level.AnimationTime for timing. }
  TThunderEffect = class
  private
    LastBeginTime, NextBeginTime: Single;
  public
    { This will init GL_LIGHT_LightNumber properties.

      Note that this requires that current matrix is modelview.
      Matrix @italic(may) be reset to identity by this procedure. }
    procedure InitGLLight(LightNumber: Cardinal);
    procedure Render(LightNumber: Cardinal);

    { If ThunderEffect <> nil, this just calls ThunderEffect.Render(LightNumber).
      If no, this disables OpenGL light number LightNumber.
      In effect, this *always* enables or disables
      OpenGL light number LightNumber. }
    class procedure RenderOrDisable(
      ThunderEffect: TThunderEffect; LightNumber: Cardinal);

    procedure Idle;
    { Force thunder happening in next Idle call. }
    procedure ForceNow;
  end;

implementation

uses ALSourceAllocator, OpenGLh, KambiGLUtils, CastleSound, CastlePlay;

procedure TThunderEffect.InitGLLight(LightNumber: Cardinal);
var
  GLLight: TGLenum;
begin
  GLLight := GL_LIGHT0 + LightNumber;

  { GL_POSITION of the light is affected by current matrix
    (i.e. current at the time of glLightv(GLLight, GL_POSITION, ...) call).
    So it's safest to set here matrix. }
  glLoadIdentity;

  { Prepare "thunder light" }
  glLightv(GLLight, GL_POSITION, Vector4f(0, 1, -1, 0));
  glLightv(GLLight, GL_AMBIENT, Vector4f(0.5, 0.5, 1, 1));
  glLightv(GLLight, GL_DIFFUSE, Vector4f(0.5, 0.5, 1, 1));
  glLightv(GLLight, GL_SPECULAR, Vector4f(0.5, 0.5, 1, 1));
  { No spot. }
  glLighti(GLLight, GL_SPOT_CUTOFF, 180);
end;

procedure TThunderEffect.Render(LightNumber: Cardinal);
var
  GLLight: TGLenum;
  ThunderTime: Single;
  ThunderVisible: boolean;
begin
  { calculate GLLight }
  GLLight := GL_LIGHT0 + LightNumber;

  { calculate ThunderVisible }
  ThunderVisible := false;
  if LastBeginTime <> 0 then
  begin
    ThunderTime := Level.AnimationTime - LastBeginTime;
    if (ThunderTime < 1.0) or
       ((1.5 < ThunderTime) and (ThunderTime < 2.5)) then
      ThunderVisible := true;
  end;

  if ThunderVisible then
    glEnable(GLLight) else
    glDisable(GLLight);
end;

class procedure TThunderEffect.RenderOrDisable(
  ThunderEffect: TThunderEffect; LightNumber: Cardinal);
begin
  if ThunderEffect <> nil then
    ThunderEffect.Render(LightNumber) else
    glDisable(GL_LIGHT0 + LightNumber);
end;

procedure TThunderEffect.Idle;
begin
  if NextBeginTime = 0 then
    NextBeginTime := Level.AnimationTime + 10 + Random(10);

  if NextBeginTime <= Level.AnimationTime then
  begin
    LastBeginTime := Level.AnimationTime;
    NextBeginTime := Level.AnimationTime + 10 + Random(20);

    {ThunderAllocatedSound := }Sound(stThunder);
  end;
end;

procedure TThunderEffect.ForceNow;
begin
  NextBeginTime := Level.AnimationTime;
end;

end.