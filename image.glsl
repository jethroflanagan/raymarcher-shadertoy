// ref: Art of Code https://www.youtube.com/watch?v=AfKGMUDWfuE
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

vec3 opRep( in vec3 p, float c )
{
    return mod(p + 0.5 * c, c) - 0.5 * c;
}

vec3 opRep( in vec3 p, vec3 c )
{
    return mod(p + 0.5 * c, c) - 0.5 * c;
}

// polynomial, taken from Dreams (ref: https://www.iquilezles.org/www/articles/smin/smin.htm)
float smoothMinPoly(float a, float b, float smoothness) {
  float h = max( smoothness-abs(a-b), 0.0 )/smoothness;
  return min( a, b ) - h*h*smoothness*(1.0/4.0);
}
// exponential, ref: https://www.iquilezles.org/www/articles/smin/smin.htm
float smoothMinExpo(float a, float b, float smoothness) {
  float res = exp2( -smoothness*a ) + exp2( -smoothness*b );
    return -log2( res )/smoothness;
}

// power, ref: https://www.iquilezles.org/www/articles/smin/smin.htm
float smoothMinPow(float a, float b, float smoothness) {
  a = pow(a, smoothness); b = pow(b, smoothness);
  return pow((a * b) / (a + b), 1.0 / smoothness);
}

float booleanSubtract(float distanceA, float distanceB) {
  return max(-distanceB, distanceA);
}

float booleanIntersection(float distanceA, float distanceB) {
  return max(distanceB, distanceA);
}

float booleanUnion(float distanceA, float distanceB) {
  return min(distanceB, distanceA);
}

float booleanUnionSmooth(float distanceA, float distanceB, float smoothness) {
  return smoothMinPoly(distanceB, distanceA, smoothness);
}


Sphere signedDistanceSphere(vec3 point, vec3 position, float radius) {
  float distance = length(opRep(point - position, 10.)) - radius;
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

Torus signedDistanceTorus(vec3 point, vec3 position, float radius, float innerRadius, float offset) {
  vec3 relativePosition = point - position;
  // relativePosition.z = mod(relativePosition.z, 3.);
  // relativePosition.xy += vec2(sin(iTime), cos(iTime) * 2.);
  // float c = 10.;
  relativePosition = opRep(relativePosition, vec3(10., 10., 0.));
  relativePosition.xy *= rotate(sin(iTime) + offset);
  relativePosition.zy *= rotate(iTime + offset);
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
  boxPosition.yz *= rotate(iTime*.4);
  boxPosition.yx *= rotate(iTime*.4);
  float stepDivision = 4.; // to cater for rotation causing artifacts
  float distance = length(max(abs(mod(boxPosition, 10.)) - size, 0.)) / stepDivision;
  return Box(
    position,
    size,
    distance
  );
}

float getDistanceToScene(vec3 point) {
    Sphere sphere = signedDistanceSphere(point, vec3(0, 1, 0), 1.);
    Sphere sphere2 = signedDistanceSphere(point, vec3(1., 1, 0), 1.);

    Capsule capsule = signedDistanceCapsule(point, vec3(0, .7, 0), vec3(1, 2, 0), .2);

    Cylinder cylinder = signedDistanceCylinder(point, vec3(1.5, .1, 2), vec3(.1, 1.2, 2), .4);

    Torus torus = signedDistanceTorus(point, vec3(0, 0.2, 1), 1.5, .2, 0.);
    Torus torus2 = signedDistanceTorus(point, vec3(0, 0.2, 1), .7, .1, 1.);

    Box box = signedDistanceBox(point, vec3(-1, .15, 0), vec3(.5, .3, 1.9));

    // boolean modifications
    // float distance = min(torus.distance, sphere.distance);

    float sDistance = booleanUnionSmooth(torus.distance, sphere.distance, .5);
    // sDistance = booleanUnionSmooth(sDistance, capsule.distance, 0.3);
    // sDistance = booleanUnionSmooth(sDistance, torus.distance, 0.3);
    // sDistance = booleanUnionSmooth(sDistance, torus2.distance, 0.3);
    // sDistance = booleanUnionSmooth(sDistance, cylinder.distance, 0.3);

    // scene distance
    float distanceToPlane = point.y;
    float distance = booleanUnionSmooth(sDistance, distanceToPlane, .5);
    // float distance = min(capsule.distance, distanceToPlane);
    // distance = min(distance, sphere2.distance);
    // distance = min(distance, sphere.distance);
    // distance = min(distance, box.distance);
    // distance = min(distance, sDistance);
    // distance = min(distance, torus.distance);
    // distance = min(distance, cylinder.distance);

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
    rayOrigin.xz *= rotate(- mousePosition.x*6.2831);

    // vec3 rayDistance = normalize(vec3(uv.x, uv.y - .2, 1));
    vec3 rayDistance = R(uv, rayOrigin, vec3(0), .7);

    float distance = rayMarch(rayOrigin, rayDistance);

    vec3 point = rayOrigin + rayDistance * distance;
    float diffuseLight = getLight(point);

    vec3 col = vec3(diffuseLight, 0., 0.4);
    // vec3 col = getNormal(point);

    fragColor = vec4(col, 1.0);
}
