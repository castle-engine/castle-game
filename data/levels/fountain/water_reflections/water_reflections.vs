/* For some more comments, see water_reflections_normalmap.vs
   in demo_models. This is adjusted to work nicely with
   water on fountain level. */

/* Varying vectors below are given in in eye-space.
   Always normalized, to make "fair" interpolation. */
varying vec3 vertex_to_camera;
varying vec3 vertex_to_light;

varying vec3 normal;

void main(void)
{
  gl_Position = ftransform();

  vec3 light_position = vec3(0.0, 0.0, 0.0);
  vec3 vertex = vec3(gl_ModelViewMatrix * gl_Vertex);
  vertex_to_light = normalize(light_position - vertex);
  /* That's easy, since in eye space camera position is always (0, 0, 0). */
  vertex_to_camera = normalize(- vertex);

  normal = normalize(gl_NormalMatrix * gl_Normal);
}
