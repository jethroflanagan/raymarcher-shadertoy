#define SURFACE_DISTANCE_EPSILON 0.01
#define MAX_STEPS 100
#define MAX_DISTANCE 100.

struct Sphere {
  vec3 position;
  float radius;
  float distance;
};

// 2 spheres + cylinder between
struct Capsule {
  vec3 a;
  vec3 b;
  float radius;
  float distance;
};

struct Torus {
  vec3 position;
  float innerRadius;
  float radius;
  float distance;
};


Sphere signedDistanceSphere(vec3 point, vec3 position, float radius) {
  float distance = length(point - position) - radius;
  return Sphere(position, radius, distance);
}

Capsule signedDistanceCapsule(vec3 point, vec3 a, vec3 b, float radius) {
  vec3 ab = b - a;
  vec3 ap = point - a;

  float t = dot(ab, ap) / dot(ab, ab);
  t = clamp(t, 0., 1.);
  vec3 c = a + t * ab;

  float distance = length(point - c) - radius;

  return Capsule(
    a, b, radius,
    distance
  );
}

Torus signedDistanceTorus(vec3 point, vec3 position, float radius, float innerRadius) {
  vec3 relativePosition = point - position;
  float x = length(relativePosition.xz) - radius;
  float distance = length(vec2(x, relativePosition.y)) - innerRadius;
  return Torus(
    position,
    innerRadius,
    radius,
    distance
  );
}

float getDistanceToScene(vec3 point) {
    Sphere sphere = signedDistanceSphere(point, vec3(0, 1, 6), 1.);

    Capsule capsule = signedDistanceCapsule(point, vec3(0, 1, 6), vec3(1, 2, 6), .2);

    Torus torus = signedDistanceTorus(point, vec3(0, 0.2, 5), 1.5, .1);

    float distanceToPlane = point.y;
    float distance = min(capsule.distance, distanceToPlane);
    distance = min(distance, torus.distance);

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
  lightPosition.xz += vec2(sin(iTime), cos(iTime) * 2.);

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

