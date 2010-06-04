/* For some more comments, see water_reflections_normalmap.fs
   in kambi_vrml_test_suite. This is adjusted to work nicely with
   water on fountain level. */

uniform samplerCube envMap;
uniform sampler2D normalMap;
uniform mat3 cameraRotationInverseMatrix;

varying vec3 vertex_to_camera;
varying vec3 vertex_to_light;
varying mat3 normal_matrix;

void main(void)
{
  vec3 normal = texture2D(normalMap, gl_TexCoord[0].st).xyz;
  normal.xy = normal.xy * 2.0 - vec2(1.0, 1.0);
  normal = normal_matrix * normal;

  float diffuse = dot(normalize(vertex_to_light), normal);
  /* We fake the light has always more intensity.
     This isn't correct, but makes it look better. */
  diffuse = (diffuse + 1.0) / 2.0;
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
