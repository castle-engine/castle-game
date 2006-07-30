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
unit GLHeadLight;

interface

uses VectorMath;

const
  DefaultHeadlightAmbientColor: TVector4Single = (0, 0, 0, 1);
  DefaultHeadlightDiffuseColor: TVector4Single = (1, 1, 1, 1);
  DefaultHeadlightSpecularColor: TVector4Single = (1, 1, 1, 1);
  DefaultHeadlightAttenuation: TVector3Single = (1, 0, 0);
  DefaultHeadlightSpot = false;
  DefaultHeadlightSpotCutoff = 180;
  DefaultHeadlightSpotExponent = 0.0;

type
  TGLHeadLight = class
  private
    FAmbientColor: TVector4Single;
    FDiffuseColor: TVector4Single;
    FSpecularColor: TVector4Single;
    FAttenuation: TVector3Single;
    FSpot: boolean;
    FSpotCutoff: Single;
    FSpotExponent: Single;
  public
    constructor Create;
    destructor Destroy; override;

    { Default value is DefaultHeadlightAmbientColor.}
    property AmbientColor: TVector4Single read FAmbientColor write FAmbientColor;
    { Default value is DefaultHeadlightDiffuseColor.}
    property DiffuseColor: TVector4Single read FDiffuseColor write FDiffuseColor;
    { Default value is DefaultHeadlightSpecularColor. }
    property SpecularColor: TVector4Single read FSpecularColor write FSpecularColor;
    { Default value is DefaultHeadlightAttenuation. }
    property Attenuation: TVector3Single read FAttenuation write FAttenuation;
    property Spot: boolean read FSpot write FSpot
      default DefaultHeadlightSpot;
    property SpotCutoff: Single read FSpotCutoff write FSpotCutoff
      default DefaultHeadlightSpotCutoff;
    property SpotExponent: Single read FSpotExponent write FSpotExponent
      default DefaultHeadlightSpotExponent;

    { This sets properties of GL_LIGHT_GLLightNumber to render given light.

      Note that this requires that current matrix is modelview.
      Matrix @italic(may) be reset to identity by this procedure.

      If CallEnabled then it will also call glEnable(GL_LIGHT_GLLightNumber). }
    procedure Render(GLLightNumber: Cardinal; CallEnabled: boolean);

    { This is like Light.Render(GLLightNumber, true), but will call
      glDisable(GL_LIGHT_GLLightNumber) if Light is nil.

      In effect, you can call this procedure with nil or non-nil
      parameter, and you can be sure that enabled/disabled state
      of light GL_LIGHT_GLLightNumber will be set. }
    class procedure RenderOrDisable(Light: TGLHeadlight; GLLightNumber: Cardinal);
  end;

implementation

uses OpenGLh, KambiGLUtils;

constructor TGLHeadLight.Create;
begin
  inherited;

  FAmbientColor := DefaultHeadlightAmbientColor;
  FDiffuseColor := DefaultHeadlightDiffuseColor;
  FSpecularColor := DefaultHeadlightSpecularColor;
  FAttenuation := DefaultHeadlightAttenuation;
  FSpot := DefaultHeadlightSpot;
  FSpotCutoff := DefaultHeadlightSpotCutoff;
  FSpotExponent := DefaultHeadlightSpotExponent;
end;

destructor TGLHeadLight.Destroy;
begin
  inherited;
end;

procedure TGLHeadLight.Render(GLLightNumber: Cardinal; CallEnabled: boolean);
var
  GLLight: TGLenum;
begin
  GLLight := GL_LIGHT0 + GLLightNumber;

  { GL_POSITION of the light is affected by current matrix
    (i.e. current at the time of glLightv(GLLight, GL_POSITION, ...) call).
    This is headlight, so this always wants to be relative to identity
    matrix. }
  glLoadIdentity;

  if Spot then
    glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 0, 1)) else
    { The light is directional }
    glLightv(GLLight, GL_POSITION, Vector4Single(0, 0, 1, 0));

  glLightv(GLLight, GL_AMBIENT, AmbientColor);
  glLightv(GLLight, GL_DIFFUSE, DiffuseColor);
  glLightv(GLLight, GL_SPECULAR, SpecularColor);
  glLightf(GLLight, GL_CONSTANT_ATTENUATION, Attenuation[0]);
  glLightf(GLLight, GL_LINEAR_ATTENUATION, Attenuation[1]);
  glLightf(GLLight, GL_QUADRATIC_ATTENUATION, Attenuation[2]);
  if Spot then
  begin
    glLightf(GLLight, GL_SPOT_CUTOFF, SpotCutoff);
    glLightf(GLLight, GL_SPOT_EXPONENT, SpotExponent);
  end else
    glLighti(GLLight, GL_SPOT_CUTOFF, 180);

  if CallEnabled then
    glEnable(GLLight);
end;

class procedure TGLHeadLight.RenderOrDisable(Light: TGLHeadlight;
  GLLightNumber: Cardinal);
begin
  if Light <> nil then
    Light.Render(GLLightNumber, true) else
    glDisable(GL_LIGHT0 + GLLightNumber);
end;

end.