#include <cmath>   // smallpt, a Path Tracer by Kevin Beason, 2008
#include <cstdlib> // Make : g++ -O3 -fopenmp smallpt.cpp -o smallpt
#include <cstdio>  //        Remove "-fopenmp" for g++ version < 4.2
#include <omp.h>

#include <stack>
#include <vector>
#include <cassert>

struct Rand48
{
  double drand()
  {
    update();
    return (stat & 0xFFFFFFFFFFFF) * (1.0 / 281474976710656.0);
  }
  long lrand()
  {
    update();
    return (long)(stat >> 17) & 0x7FFFFFFF;
  }
  long mrand()
  {
    update();
    return (long)(stat >> 16) & 0xFFFFFFFF;
  }
  void srand(const long seed) { stat = (seed << 16) + 0x330E; }
  Rand48(const long seed = 0) { srand48(seed); }

private:
  long long stat;
  void update() { stat = stat * 0x5DEECE66D + 0xB; }
};

#if 0
using real = double;
#define EPS (1.0e-4)
#define RR  (1.0e5)
#else
using real = float;
#define EPS (1.0e-2)
#define RR  (1.0e4)
#endif

struct Vec 
{        // Usage: time ./smallpt 5000 && xv image.ppm
  real x, y, z;                  // position, also color (r,g,b)
  Vec(real x_=0, real y_=0, real z_=0) : x(x_), y(y_), z(z_) {}
  Vec operator+(const Vec &b) const { return Vec(x+b.x,y+b.y,z+b.z); }
  Vec operator-(const Vec &b) const { return Vec(x-b.x,y-b.y,z-b.z); }
  Vec operator*(const real b) const { return Vec(x*b,y*b,z*b); }
  Vec mult(const Vec &b) const { return Vec(x*b.x,y*b.y,z*b.z); }
  Vec& norm() { return *this = *this * (1/sqrt(x*x+y*y+z*z)); }
  real dot(const Vec &b) const { return x*b.x+y*b.y+z*b.z; } // cross:
  Vec operator%(Vec&b){return Vec(y*b.z-z*b.y,z*b.x-x*b.z,x*b.y-y*b.x);}
};

struct Ray 
{ 
  Vec o, d; 
  Ray(Vec o_, Vec d_) : o(o_), d(d_) {} 
};

enum class Refl_t 
{ 
  DIFF, 
  SPEC, 
  REFR 
};  // material types, used in radiance()

struct Sphere 
{
  real rad;       // radius
  Vec p, e, c;      // position, emission, color
  Refl_t refl;      // reflection type (DIFFuse, SPECular, REFRactive)
  Sphere(real rad_, Vec p_, Vec e_, Vec c_, Refl_t refl_):
    rad(rad_), p(p_), e(e_), c(c_), refl(refl_) {}

  real intersect(const Ray &r) const 
  { // returns distance, 0 if nohit
    Vec op = p-r.o; // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
    real t;
    const real eps=EPS;
    const real b=op.dot(r.d);
    real det=b*b-op.dot(op)+rad*rad;
    if (det<0) 
      return 0; 
    else 
      det=sqrt(det);
    return (t=b-det)>eps ? t : ((t=b+det)>eps ? t : 0);
  }
};
Sphere spheres[] = 
{//Scene: radius, position, emission, color, material
  Sphere((RR), Vec( (RR)+1,40.8,81.6), Vec(),Vec(.75,.25,.25),Refl_t::DIFF),//Left
  Sphere((RR), Vec(-(RR)+99,40.8,81.6),Vec(),Vec(.25,.25,.75),Refl_t::DIFF),//Rght
  Sphere((RR), Vec(50,40.8, (RR)),     Vec(),Vec(.75,.75,.75),Refl_t::DIFF),//Back
  Sphere((RR), Vec(50,40.8,-(RR)+170), Vec(),Vec(),           Refl_t::DIFF),//Frnt
  Sphere((RR), Vec(50, (RR), 81.6),    Vec(),Vec(.75,.75,.75),Refl_t::DIFF),//Botm
  Sphere((RR), Vec(50,-(RR)+81.6,81.6),Vec(),Vec(.75,.75,.75),Refl_t::DIFF),//Top
  Sphere(16.5,Vec(27,16.5,47),       Vec(),Vec(1,1,1)*.999, Refl_t::SPEC),//Mirr
  Sphere(16.5,Vec(73,16.5,78),       Vec(),Vec(1,1,1)*.999, Refl_t::REFR),//Glas
  Sphere(600, Vec(50,681.6-.27,81.6),Vec(12,12,12),  Vec(), Refl_t::DIFF) //Lite
};

using vec_t = Vec;
static inline real dot(const vec_t &a, const vec_t &b) { return a.x*b.x + a.y*b.y + a.z*b.z; }

static real intersect(const Sphere &s, const Ray &r) 
{
  const vec_t op   = s.p - r.o;
  const real b    = dot(op,r.d);
  const real det2 = b*b - dot(op,op) + s.rad*s.rad;
  
  const real eps = 1.0e-4f;

  const real det = det2 > 0.0f ? sqrt(det2) : 0.0f;
  const real tm  = b - det;
  const real tp  = b + det;
  const real t0  = tm > eps ? tm : tp;
  const real t  =  t0 > eps ? t0 : 0.0f;
  return det > 0.0f ? t : 0.0f;
}

static inline bool intersect(const Ray &r, real &t, int &id)
{
  const real n=sizeof(spheres)/sizeof(Sphere);
  real d;
  real inf=t=1e20;
  for (int i=int(n); i--; ) 
    if ((d = spheres[i].intersect(r)) && d<t)
    {
      t=d;
      id=i;
    }
  return t<inf;
}

Vec radiance(const Ray &r_, int depth_, Rand48 &rr)
{
  real t;                               // distance to intersection
  int id=0;                               // id of intersected object
  Ray r=r_;
  int depth=depth_;
  // L0 = Le0 + f0*(L1)
  //    = Le0 + f0*(Le1 + f1*L2)
  //    = Le0 + f0*(Le1 + f1*(Le2 + f2*(L3))
  //    = Le0 + f0*(Le1 + f1*(Le2 + f2*(Le3 + f3*(L4)))
  //    = ...
  //    = Le0 + f0*Le1 + f0*f1*Le2 + f0*f1*f2*Le3 + f0*f1*f2*f3*Le4 + ...
  // 
  // So:
  // F = 1
  // while (1){
  //   L += F*Lei
  //   F *= fi
  // }
  Vec cl(0,0,0);   // accumulated color
  Vec cf(1,1,1);  // accumulated reflectance
  while (1){
    if (!intersect(r, t, id)) return cl; // if miss, return black
    const Sphere &obj = spheres[id];        // the hit object
    Vec x=r.o+r.d*t, n=(x-obj.p).norm(), nl=n.dot(r.d)<0?n:n*-1, f=obj.c;
    real p = f.x>f.y && f.x>f.z ? f.x : f.y>f.z ? f.y : f.z; // max refl
    cl = cl + cf.mult(obj.e);
    if (++depth>5) if (rr.drand()<p) f=f*(1/p); else return cl; //R.R.
    cf = cf.mult(f);
    if (obj.refl == Refl_t::DIFF){                  // Ideal DIFFUSE reflection
      real r1=2*M_PI*rr.drand(), r2=rr.drand(), r2s=sqrt(r2);
      Vec w=nl, u=((fabs(w.x)>.1?Vec(0,1):Vec(1))%w).norm(), v=w%u;
      Vec d = (u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2)).norm();
      r = Ray(x,d);
      continue;
    } else if (obj.refl == Refl_t::SPEC){           // Ideal SPECULAR reflection
      r = Ray(x,r.d-n*2*n.dot(r.d));
      continue;
    }
    Ray reflRay(x, r.d-n*2*n.dot(r.d));     // Ideal dielectric REFRACTION
    bool into = n.dot(nl)>0;                // Ray from outside going in?
    real nc=1, nt=1.5, nnt=into?nc/nt:nt/nc, ddn=r.d.dot(nl), cos2t;
    if ((cos2t=1-nnt*nnt*(1-ddn*ddn))<0){    // Total internal reflection
      r = reflRay;
      continue;
    }
    Vec tdir = (r.d*nnt - n*((into?1:-1)*(ddn*nnt+sqrt(cos2t)))).norm();
    real a=nt-nc, b=nt+nc, R0=a*a/(b*b), c = 1-(into?-ddn:tdir.dot(n));
    real Re=R0+(1-R0)*c*c*c*c*c,Tr=1-Re,P=.25+.5*Re,RP=Re/P,TP=Tr/(1-P);
    if (rr.drand()<P){
      cf = cf*RP;
      r = reflRay;
    } else {
      cf = cf*TP;
      r = Ray(x,tdir);
    }
    continue;
  }
}

static inline real clamp(real x)
{ 
  return x<0 ? 0 : x>1 ? 1 : x; 
}

static inline int toInt(real x)
{ 
  return int(pow(clamp(x),1/2.2)*255+.5); 
}


int main(int argc, char *argv[])
{
  int w = 512;                                               // width
  int h = 384;                                               // hight
  int samps = argc == 2 ? atoi(argv[1]) / 4 : 1;             // # samples
  Ray cam(Vec(50, 52, 295.6), Vec(0, -0.042612, -1).norm()); // cam pos, dir

  Vec cx=Vec(w*.5135f/h);
  Vec cy=(cx%cam.d).norm()*.5135f;
  Vec r;
  Vec *c=new Vec[w*h];

  for (int s = 0; s < samps; s++)
  {
    fprintf(stderr, "\rRendering (%d spp) : %d ", samps * 4, s * 4);

#pragma omp parallel for schedule(guided)
    for (int y = 0; y < h; y++)
      for (unsigned short x = 0; x < w; x++) // Loop cols
      {
        static Rand48 rr(omp_get_thread_num());
        Vec r = Vec();
        const int idx = (h - y - 1) * w + x;
        for (int sy = 0; sy < 2; sy++) // 2x2 subpixel rows
          for (int sx = 0; sx < 2; sx++)
          { // 2x2 subpixel cols
            real r1 = 2 * rr.drand(),
                 dx = r1 < 1 ? sqrt(r1) - 1 : 1 - sqrt(2 - r1);
            real r2 = 2 * rr.drand(),
                 dy = r2 < 1 ? sqrt(r2) - 1 : 1 - sqrt(2 - r2);
            Vec d = cx * (((sx + .5 + dx) / 2 + x) / w - .5) +
                    cy * (((sy + .5 + dy) / 2 + y) / h - .5) + cam.d;
            r = r +
                radiance(Ray(cam.o + d * 140, d.norm()), 0, rr); //*(1./samps);
          }
        c[idx] = c[idx] + r;
      }
  }

  for (int i = 0; i < w*h; i++)
  {
    c[i] = c[i]*(0.25f/samps); ///samps);
    c[i] = Vec(clamp(c[i].x),clamp(c[i].y),clamp(c[i].z));
  }

  FILE *f = fopen("image.ppm", "w");         // Write image to PPM file.
  fprintf(f, "P3\n%d %d\n%d\n", w, h, 255);
  for (int i=0; i<w*h; i++)
    fprintf(f,"%d %d %d ", toInt(c[i].x), toInt(c[i].y), toInt(c[i].z));
}
