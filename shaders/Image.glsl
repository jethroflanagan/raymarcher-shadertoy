#version 330

#include <common.glsl>

uniform vec2 iResolution;
uniform float iTime;
uniform float iTimeDelta;
uniform int iFrame;
uniform vec4 iMouse;
uniform sampler2D iChannel0;
uniform sampler2D iChannel1;
uniform sampler2D iChannel2;
uniform sampler2D iChannel3;
out vec4 shadertoy_outcolor;

#define SURFACE_DISTANCE_EPSILON 0.01
#define MAX_STEPS 100
#define MAX_DISTANCE 100.

float getDistanceToScene(vec3 point) {
    vec4 sphere = vec4(0, 1, 6, 1);
    float distanceToSphere = length(point - sphere.xyz) - sphere.w;

    float distanceToPlane = point.y;
    float distance = min(distanceToSphere, distanceToPlane);

    return distance;
}

vec3 getNormal(vec3 point) {
  vec2 epsilon = vec2(.01, 0);
  float distance = getDistanceToScene(point);

  // swizzles
  vec3 normal = distance - vec3(
    getDistanceToScene(point - epsilon.xyy),
    getDistanceToScene(point - epsilon.yxy),
    getDistanceToScene(point - epsilon.yyx)
  );

  return normalize(normal);
}

float rayMarch(vec3 rayOrigin, vec3 rayDistance) {
    float distanceFromOrigin = 0.;

    for (int i = 0; i < MAX_STEPS; i++) {
        vec3 point = rayOrigin + distanceFromOrigin * rayDistance;
        float distanceToScene = getDistanceToScene(point);

        distanceFromOrigin += distanceToScene;

        if (distanceToScene < SURFACE_DISTANCE_EPSILON || distanceFromOrigin > MAX_DISTANCE) {
            break;
        }
    }
    return distanceFromOrigin;
}

float getLight(vec3 point) {
  vec3 lightPosition = vec3(0, 5, 6);
  lightPosition.xz += vec2(sin(iTime), cos(iTime));

  vec3 light = normalize(lightPosition - point);

  vec3 normal = getNormal(point);

  float diffuse = clamp(dot(normal, light), 0., 1.);

  // calculate shadows
  float distance = rayMarch(point + normal * SURFACE_DISTANCE_EPSILON * 2., light);
  if (distance < length(lightPosition - point)) {
    diffuse *= .1;
  }

  return diffuse;
}


void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = (fragCoord - .5 * iResolution.xy) / iResolution.y;
    
    vec3 rayOrigin = vec3(0, 1, 0);
    vec3 rayDistance = normalize(vec3(uv.x, uv.y, 1));

    float distance = rayMarch(rayOrigin, rayDistance);

    vec3 point = rayOrigin + rayDistance * distance;
    float diffuseLight = getLight(point);

    vec3 col = vec3(diffuseLight);
    // vec3 col = getNormal(point);

    fragColor = vec4(col, 1.0);
}


void main()
{
	mainImage(shadertoy_outcolor, gl_FragCoord.xy);
}