{
  Copyright 2010 Michalis Kamburelis.

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

{ Tricks with precalculated animation for "The Castle". }
unit CastleAnimationTricks;

interface

uses VRMLGLScene, Classes, VRMLGLAnimation, Frustum, VectorMath;

type
  { Animation forced to seamlessly loop by blending the beginning frames
    with end frames. This is a really brutal (often looking bad),
    but universal way to make animation seamlessly loop (works
    even with animations that are not structurally equal, so meshes cannot
    be interpolated etc. in a usual way).

    Note that the normal blending control (for rendering transparent materials)
    has to be disabled for this, as this requires full control over blending.
    Also normal materials control must be disabled
    (we have to supply our own alpha value).
    So Attributes.Blending and Attributes.ControlBlending and
    Attributes.ControlMaterials must always remain @false.

    This also ignores TimeLoop (works like it's always @true) and
    TimeBackwards (works like it's always @false). }
  TBlendedLoopingAnimation = class(TVRMLGLAnimation)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Render(const Frustum: TFrustum;
      TransparentGroup: TTransparentGroup;
      InShadow: boolean); override;
  end;

implementation

uses Math, KambiUtils, GL, KambiGLUtils;

constructor TBlendedLoopingAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Attributes.Blending := false;
  Attributes.ControlMaterials := false;
  Attributes.ControlBlending := false;
end;

procedure TBlendedLoopingAnimation.Render(const Frustum: TFrustum;
  TransparentGroup: TTransparentGroup; InShadow: boolean);
var
  SceneIndex, MiddleIndex, HalfIndex: Integer;
  Amount: Single;
begin
  if Loaded and Exists then
  begin
    SceneIndex := Floor(MapRange(Time, TimeBegin, TimeEnd, 0, ScenesCount)) mod ScenesCount;
    if SceneIndex < 0 then SceneIndex += ScenesCount; { we wanted "unsigned mod" above }
    MiddleIndex := ScenesCount div 2;

    glPushAttrib(GL_COLOR_BUFFER_BIT or GL_LIGHTING_BIT);
    try
      glEnable(GL_BLEND); // saved by GL_COLOR_BUFFER_BIT
      glBlendFunc(GL_SRC_ALPHA, GL_ONE); // saved by GL_COLOR_BUFFER_BIT

      { calculate Amount.

        On TimeBegin (ModResult = 0) and
        TimeEnd (ModResult = ScenesCount - 1), it's 0.
        Exactly in the middle (ModResult = MiddleIndex), it's 1.
        Between, it's linearly interpolated.
        This is the visibility of the 1st (unshifted) copy of animation.
        Since it's not visible at TimeBegin and TimeEnd, the looping seam
        is not visible.

        The second (shifted) copy of the animation has always visibility
        1-Amount. And it's shifted by half time range (MiddleIndex).
        This way the seam happens at MiddleIndex, when the shifted animation
        is not visible, so the looping seam is again not visible. }
      if SceneIndex >= MiddleIndex then
        HalfIndex := MiddleIndex - 1 - (SceneIndex - MiddleIndex) else
        HalfIndex := SceneIndex;
      Assert((0 <= HalfIndex) and (HalfIndex < MiddleIndex));
      Amount := HalfIndex / (MiddleIndex - 1);

      glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 1, Amount)); // saved by GL_LIGHTING_BIT
      Scenes[SceneIndex].Render(Frustum, TransparentGroup, InShadow);

      glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Vector4Single(1, 1, 1, 1 - Amount)); // saved by GL_LIGHTING_BIT
      Scenes[(SceneIndex + MiddleIndex) mod ScenesCount].Render(Frustum, TransparentGroup, InShadow);
    finally glPopAttrib end;
  end;
end;

end.
