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

enum Refl_t 
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
  Sphere((RR), Vec( (RR)+1,40.8,81.6), Vec(),Vec(.75,.25,.25),DIFF),//Left
  Sphere((RR), Vec(-(RR)+99,40.8,81.6),Vec(),Vec(.25,.25,.75),DIFF),//Rght
  Sphere((RR), Vec(50,40.8, (RR)),     Vec(),Vec(.75,.75,.75),DIFF),//Back
  Sphere((RR), Vec(50,40.8,-(RR)+170), Vec(),Vec(),           DIFF),//Frnt
  Sphere((RR), Vec(50, (RR), 81.6),    Vec(),Vec(.75,.75,.75),DIFF),//Botm
  Sphere((RR), Vec(50,-(RR)+81.6,81.6),Vec(),Vec(.75,.75,.75),DIFF),//Top
  Sphere(16.5,Vec(27,16.5,47),       Vec(),Vec(1,1,1)*.999, SPEC),//Mirr
  Sphere(16.5,Vec(73,16.5,78),       Vec(),Vec(1,1,1)*.999, REFR),//Glas
  Sphere(600, Vec(50,681.6-.27,81.6),Vec(12,12,12),  Vec(), DIFF) //Lite
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

static Vec radiance(Ray r, int depth, unsigned short *Xi, Rand48 &rr)
{
  using pair1 = std::pair<Ray,int>;
  using pair2 = std::pair<Vec,Vec>;

  std::vector<pair2> partial;

  partial.reserve(128);

  constexpr int ndepth = 32;
  for (int depth = 0; depth < ndepth; depth++)
  {
    real t;                               // distance to intersection
    int id=0;                               // id of intersected object
    if (!intersect(r, t, id)) 
    {
      break;
    }

    const Sphere &obj = spheres[id];        // the hit object

    Vec x = r.o+r.d*t;
    Vec n = (x-obj.p).norm();
    Vec nl = n.dot(r.d) <0 ? n : n*(-1);
    Vec f=obj.c;
    real p = (f.x>f.y && f.x>f.z) ? f.x : (f.y>f.z ? f.y : f.z); // max refl

    if (depth>=5) 
    {
      if (rr.drand()<p) 
      {
        f=f*(1/p); 
      }
      else 
      {
        partial.emplace_back(obj.e, Vec());
        break;
      }
    }

    if (obj.refl == SPEC)            // Ideal SPECULAR reflection
    {
      partial.emplace_back(obj.e, f);
      r = Ray(x,r.d-n*2*n.dot(r.d));
    }
    else if (obj.refl == DIFF)
    {                  // Ideal DIFFUSE reflection
      real r1=2*M_PI*rr.drand();
      real r2=rr.drand();
      real r2s=sqrt(r2);

      Vec w=nl;
      Vec u=( (fabs(w.x)>.1 ? Vec(0,1) : Vec(1))%w ).norm(); 
      Vec v=w%u;
      Vec d = (u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2)).norm();

      partial.emplace_back(obj.e, f);
      r = Ray(x,d);
    } 
    else
    {
      Ray reflRay(x, r.d-n*2*n.dot(r.d));     // Ideal dielectric REFRACTION
      bool into = n.dot(nl)>0;                // Ray from outside going in?
      real nc=1;
      real nt=1.5;
      real nnt=into?nc/nt:nt/nc;
      real ddn=r.d.dot(nl);
      real cos2t;

      if ((cos2t=1-nnt*nnt*(1-ddn*ddn))<0)    // Total internal reflection
      {
        partial.emplace_back(obj.e,f);
        r = reflRay;
      }
      else
      {
        Vec tdir = (r.d*nnt - n*( (into?1:-1)*(ddn*nnt+sqrt(cos2t))) ).norm();

        real a=nt-nc;
        real b=nt+nc;
        real R0=a*a/(b*b);
        real c = 1-(into?-ddn:tdir.dot(n));

        real Re=R0+(1-R0)*c*c*c*c*c;
        real Tr=1-Re;
        real P = .25 + .5 * Re;
        real RP=Re/P;
        real TP=Tr/(1-P);

        if (rr.drand() < P)
        {
          partial.emplace_back(obj.e, Vec(RP,RP,RP));
          r = reflRay;
        }
        else
        {
          partial.emplace_back(obj.e, Vec(TP,TP,TP));
          r = Ray(x,tdir);
        }
      }
    }
  }

  Vec col = Vec();
  const int n = partial.size();
  for (int i = n-1; i >= 0; i--)
  {
    auto p = partial[i];
    col = p.first + p.second.mult(col);
  }
  return col;


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


  unsigned short Xi[3];
  {
    for (int s=0; s<samps; s++)
    {
      fprintf(stderr,"\rRendering (%d spp) : %d ",samps*4, s*4);

#pragma omp parallel for schedule(guided)
      for (int y=0; y<h; y++)
        for (unsigned short x=0; x<w; x++)   // Loop cols
        {
          static Rand48 rr(omp_get_thread_num());
          Vec r = Vec();
          const int idx = (h-y-1)*w+x;
          for (int sy=0; sy<2; sy++)     // 2x2 subpixel rows
            for (int sx=0; sx<2; sx++)
            {        // 2x2 subpixel cols
              real r1=2*rr.drand(), dx=r1<1 ? sqrt(r1)-1: 1-sqrt(2-r1);
              real r2=2*rr.drand(), dy=r2<1 ? sqrt(r2)-1: 1-sqrt(2-r2);
              Vec d = cx*( ( (sx+.5 + dx)/2 + x)/w - .5) +
                cy*( ( (sy+.5 + dy)/2 + y)/h - .5) + cam.d;
              r = r + radiance(Ray(cam.o+d*140,d.norm()),0,Xi,rr); //*(1./samps);
            } 
          c[idx] = c[idx] + r;
        }
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
