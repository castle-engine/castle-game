uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;
uniform mat3 castle_NormalMatrix;
attribute vec4 castle_Vertex;
attribute vec3 castle_Normal;

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
  vec4 vertex_eye = castle_ModelViewMatrix * castle_Vertex;
  gl_Position = castle_ProjectionMatrix * vertex_eye;

  vec3 light_position = vec3(0.0, 0.0, 0.0);
  vertex_to_light = normalize(light_position - vertex_eye);
  /* That's easy, since in eye space camera position is always (0, 0, 0). */
  vertex_to_camera = normalize(- vertex);

  normal = normalize(castle_NormalMatrix * castle_Normal);
}
