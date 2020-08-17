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

struct Cylinder {
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

struct Box {
  vec3 position;
  vec3 size;
  float distance;
};

mat2 rotate(float a) {
  float s = sin(a);
  float c = cos(a);
  return mat2(c, -s, s, c);
}

vec3 R(vec2 uv, vec3 p, vec3 l, float z) {
    vec3 f = normalize(l-p),
        r = normalize(cross(vec3(0,1,0), f)),
        u = cross(f,r),
        c = p+f*z,
        i = c + uv.x*r + uv.y*u,
        d = normalize(i-p);
    return d;
}

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

Cylinder signedDistanceCylinder(vec3 point, vec3 a, vec3 b, float radius) {
  vec3 ab = b - a;
  vec3 ap = point - a;

  float t = dot(ab, ap) / dot(ab, ab);
  vec3 c = a + t * ab;

  float distance = length(point - c) - radius;

  float y = (abs(t - .5) - .5) * length(ab);

  float exteriorDistance = length(max(vec2(distance, y), 0.));
  float interiorDistance = min(max(distance, y), 0.);

  float cappedDistance = interiorDistance + exteriorDistance;
  return Cylinder(
    a, b, radius,
    cappedDistance
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

Box signedDistanceBox(vec3 point, vec3 position, vec3 size) {
  vec3 boxPosition = (point - position);
  boxPosition.xz *= rotate(iTime);
  // boxPosition.yz *= rotate(iTime*.4);
  // boxPosition.yx *= rotate(iTime*.4);

  float distance = length(max(abs(boxPosition) - size, 0.));
  return Box(
    position,
    size,
    distance
  );
}

float getDistanceToScene(vec3 point) {
    Sphere sphere = signedDistanceSphere(point, vec3(0, 1, 0), 1.);

    Capsule capsule = signedDistanceCapsule(point, vec3(0, .7, 0), vec3(1, 2, 0), .2);

    Cylinder cylinder = signedDistanceCylinder(point, vec3(1.5, .1, 2), vec3(.1, 1.2, 2), .4);

    Torus torus = signedDistanceTorus(point, vec3(0, 0.2, 1), 1.5, .1);

    Box box = signedDistanceBox(point, vec3(-1, .15, 0), vec3(.5, .3, 1.9));


    float distanceToPlane = point.y;
    float distance = min(capsule.distance, distanceToPlane);
    distance = min(distance, sphere.distance);
    distance = min(distance, torus.distance);
    distance = min(distance, box.distance);
    distance = min(distance, cylinder.distance);

    return distance;
}

vec3 getNormal(vec3 point) {
  vec2 epsilon = vec2(.01, 0);
  float distance = getDistanceToScene(point);

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
  vec3 lightPosition = vec3(0, 5, 0);
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
    vec2 mousePosition = iMouse.xy / iResolution.xy;

    // camera
    vec3 rayOrigin = vec3(0, 4, -5);
    rayOrigin.yz *= rotate(-mousePosition.y + .4);
    rayOrigin.xz *= rotate(iTime*.2 - mousePosition.x*6.2831);

    // vec3 rayDistance = normalize(vec3(uv.x, uv.y - .2, 1));
    vec3 rayDistance = R(uv, rayOrigin, vec3(0), .7);

    float distance = rayMarch(rayOrigin, rayDistance);

    vec3 point = rayOrigin + rayDistance * distance;
    float diffuseLight = getLight(point);

    vec3 col = vec3(diffuseLight, 0., 0.4);
    // vec3 col = getNormal(point);

    fragColor = vec4(col, 1.0);
}

