/* For some more comments, see water_reflections_normalmap.fs
   in kambi_vrml_test_suite. This is adjusted to work nicely with
   water on fountain level. */

uniform samplerCube envMap;
uniform mat3 cameraRotationInverseMatrix;

varying vec3 vertex_to_camera;
varying vec3 vertex_to_light;
varying vec3 normal;

void main(void)
{
  float diffuse = dot(normalize(vertex_to_light), normal);
  /* We fake the light has always more intensity.
     This isn't correct, but makes it look better. */
  diffuse = (diffuse + 1.0) * 2.0;
  diffuse = max(diffuse, 0.0);

  vec3 to_camera = normalize(vertex_to_camera);

  vec3 reflected = reflect(to_camera, normal);

  reflected = cameraRotationInverseMatrix * reflected;

  vec3 reflectedColor = textureCube(envMap, -reflected).rgb;

  /* fake reflectedColor to be lighter. This just Looks Better. */
  reflectedColor *= 1.5;

  gl_FragColor.rgb = reflectedColor * vec3(gl_FrontMaterial.diffuse) * diffuse;

  float refraction_amount = dot(to_camera, normal);
  gl_FragColor.a = 1.0 - refraction_amount;
}
