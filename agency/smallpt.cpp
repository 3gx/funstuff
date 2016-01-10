#include <math.h>   // smallpt, a Path Tracer by Kevin Beason, 2008
#include <stdlib.h> // Make : g++ -O3 -fopenmp smallpt.cpp -o smallpt
#include <stdio.h>  //        Remove "-fopenmp" for g++ version < 4.2
#include <omp.h>

#include <stack>
#include <vector>
#include <cassert>

struct Vec 
{        // Usage: time ./smallpt 5000 && xv image.ppm
  double x, y, z;                  // position, also color (r,g,b)
  Vec(double x_=0, double y_=0, double z_=0) : x(x_), y(y_), z(z_) {}
  Vec operator+(const Vec &b) const { return Vec(x+b.x,y+b.y,z+b.z); }
  Vec operator-(const Vec &b) const { return Vec(x-b.x,y-b.y,z-b.z); }
  Vec operator*(const double b) const { return Vec(x*b,y*b,z*b); }
  Vec mult(const Vec &b) const { return Vec(x*b.x,y*b.y,z*b.z); }
  Vec& norm() { return *this = *this * (1/sqrt(x*x+y*y+z*z)); }
  double dot(const Vec &b) const { return x*b.x+y*b.y+z*b.z; } // cross:
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
  double rad;       // radius
  Vec p, e, c;      // position, emission, color
  Refl_t refl;      // reflection type (DIFFuse, SPECular, REFRactive)
  Sphere(double rad_, Vec p_, Vec e_, Vec c_, Refl_t refl_):
    rad(rad_), p(p_), e(e_), c(c_), refl(refl_) {}

  double intersect(const Ray &r) const 
  { // returns distance, 0 if nohit
    Vec op = p-r.o; // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
    double t;
    const double eps=1e-4;
    const double b=op.dot(r.d);
    double det=b*b-op.dot(op)+rad*rad;
    if (det<0) 
      return 0; 
    else 
      det=sqrt(det);
    return (t=b-det)>eps ? t : ((t=b+det)>eps ? t : 0);
  }
};
#define RR (1e5)
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
static inline double dot(const vec_t &a, const vec_t &b) { return a.x*b.x + a.y*b.y + a.z*b.z; }

static double intersect(const Sphere &s, const Ray &r) 
{
  const vec_t op   = s.p - r.o;
  const double b    = dot(op,r.d);
  const double det2 = b*b - dot(op,op) + s.rad*s.rad;
  
  const double eps = 1.0e-4f;

  const double det = det2 > 0.0f ? sqrt(det2) : 0.0f;
  const double tm  = b - det;
  const double tp  = b + det;
  const double t0  = tm > eps ? tm : tp;
  const double t  =  t0 > eps ? t0 : 0.0f;
  return det > 0.0f ? t : 0.0f;
}
static bool intersect(
    const Sphere spheres[],
    const int n,
    const Ray &r, 
    double &t, 
    int &id)
{
  const double inf = 1.0e10f;
  t = inf;
//  fprintf(stderr, "n= %d\n", n);
  for (int i = 0; i < n; i++)
  {
 //   fprintf(stderr, "i= %d\n", i);
    const Sphere s = spheres[i];
    const double d = intersect(s, r);
    if (d != 0.0f && d < t)
    {
      t  = d;
      id = i;
    }
  }
  return t < inf;
}


static inline double clamp(double x)
{ 
  return x<0 ? 0 : x>1 ? 1 : x; 
}

static inline int toInt(double x)
{ 
  return int(pow(clamp(x),1/2.2)*255+.5); 
}

static inline bool intersect(const Ray &r, double &t, int &id)
{
  const double n=sizeof(spheres)/sizeof(Sphere);
  double d;
  double inf=t=1e20;
  for (int i=int(n); i--; ) 
    if ((d = spheres[i].intersect(r)) && d<t)
    {
      t=d;
      id=i;
    }
  return t<inf;
}

static Vec radiance(Ray r, int depth, unsigned short *Xi)
{
#if 1
  using pair1 = std::pair<Ray,int>;
  using pair2 = std::pair<Vec,Vec>;

  std::vector<pair2> partial;

  partial.reserve(128);

  constexpr int ndepth = 32;
  int maxdepth = 0;
  for (int depth = 0; depth < ndepth; depth++)
  {
    maxdepth = depth;
    double t;                               // distance to intersection
    int id=0;                               // id of intersected object
#if 0
    if (!intersect(r, t, id)) 
      break;
#else
    if (!intersect(spheres,sizeof(spheres)/sizeof(Sphere),r,t,id))
      break;
#endif

    const Sphere &obj = spheres[id];        // the hit object

    Vec x = r.o+r.d*t;
    Vec n = (x-obj.p).norm();
    Vec nl = n.dot(r.d) <0 ? n : n*(-1);
    Vec f=obj.c;
    double p = (f.x>f.y && f.x>f.z) ? f.x : (f.y>f.z ? f.y : f.z); // max refl

    if (depth>=5) 
    {
      if (drand48()<p) 
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
      double r1=2*M_PI*drand48();
      double r2=drand48();
      double r2s=sqrt(r2);

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
      double nc=1;
      double nt=1.5;
      double nnt=into?nc/nt:nt/nc;
      double ddn=r.d.dot(nl);
      double cos2t;

      if ((cos2t=1-nnt*nnt*(1-ddn*ddn))<0)    // Total internal reflection
      {
        partial.emplace_back(obj.e,f);
        r = reflRay;
      }
      else
      {
        Vec tdir = (r.d*nnt - n*( (into?1:-1)*(ddn*nnt+sqrt(cos2t))) ).norm();

        double a=nt-nc;
        double b=nt+nc;
        double R0=a*a/(b*b);
        double c = 1-(into?-ddn:tdir.dot(n));

        double Re=R0+(1-R0)*c*c*c*c*c;
        double Tr=1-Re;
        double P=.25+.5*Re;
        double RP=Re/P;
        double TP=Tr/(1-P);

        if (drand48() < P)
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
#if 0
  static int mmm = 0;
  mmm = std::max(mmm,maxdepth);
  fprintf(stderr, "maxdepth= %d\n" , mmm);
#endif

  Vec col = Vec();
  const int n = partial.size();
  for (int i = n-1; i >= 0; i--)
  {
    auto p = partial[i];
    col = p.first + p.second.mult(col);
  }
  return col;


#else
  double t;                               // distance to intersection
  int id=0;                               // id of intersected object
  if (!intersect(r, t, id)) 
    return Vec(); // if miss, return black

  const Sphere &obj = spheres[id];        // the hit object

  Vec x = r.o+r.d*t;
  Vec n = (x-obj.p).norm();
  Vec nl = n.dot(r.d) <0 ? n : n*(-1);
  Vec f=obj.c;
  double p = (f.x>f.y && f.x>f.z) ? f.x : (f.y>f.z ? f.y : f.z); // max refl

  if (++depth>5) 
  {
    if (drand48()<p) 
      f=f*(1/p); 
    else 
      return obj.e; //R.R.
  }

  if (obj.refl == DIFF)
  {                  // Ideal DIFFUSE reflection
    double r1=2*M_PI*drand48();
    double r2=drand48();
    double r2s=sqrt(r2);

    Vec w=nl;
    Vec u=( (fabs(w.x)>.1 ? Vec(0,1) : Vec(1))%w ).norm(); 
    Vec v=w%u;
    Vec d = (u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2)).norm();
    return obj.e + f.mult(radiance(Ray(x,d),depth,Xi));
  } 
  else if (obj.refl == SPEC)            // Ideal SPECULAR reflection
    return obj.e + f.mult(radiance(Ray(x,r.d-n*2*n.dot(r.d)),depth,Xi));

  Ray reflRay(x, r.d-n*2*n.dot(r.d));     // Ideal dielectric REFRACTION
  bool into = n.dot(nl)>0;                // Ray from outside going in?
  double nc=1;
  double nt=1.5;
  double nnt=into?nc/nt:nt/nc;
  double ddn=r.d.dot(nl);
  double cos2t;

  if ((cos2t=1-nnt*nnt*(1-ddn*ddn))<0)    // Total internal reflection
    return obj.e + f.mult(radiance(reflRay,depth,Xi));

  Vec tdir = (r.d*nnt - n*( (into?1:-1)*(ddn*nnt+sqrt(cos2t))) ).norm();

  double a=nt-nc;
  double b=nt+nc;
  double R0=a*a/(b*b);
  double c = 1-(into?-ddn:tdir.dot(n));

  double Re=R0+(1-R0)*c*c*c*c*c;
  double Tr=1-Re;
  double P=.25+.5*Re;
  double RP=Re/P;
  double TP=Tr/(1-P);

  if (depth > 2)
  {
    if (drand48(Xi) < P)
    {
      return obj.e + radiance(reflRay,depth,Xi)*RP;
    }
    else
    {
      return obj.e + radiance(Ray(x,tdir),depth,Xi)*TP;
    }
  }
  else
  {
    return obj.e + radiance(reflRay,depth,Xi)*Re+radiance(Ray(x,tdir),depth,Xi)*Tr;
  }


#if 0
  return obj.e + f.mult(depth>2 ? (erand48(Xi)<P ?   // Russian roulette
    radiance(reflRay,depth,Xi)*RP:radiance(Ray(x,tdir),depth,Xi)*TP) :
    radiance(reflRay,depth,Xi)*Re+radiance(Ray(x,tdir),depth,Xi)*Tr);
#endif
#endif
}

int main(int argc, char *argv[])
{
  int w=512;
  int h=384;
  int samps = argc==2 ? atoi(argv[1])/4 : 1; // # samples
  Ray cam(Vec(50,52,295.6), Vec(0,-0.042612,-1).norm()); // cam pos, dir

  Vec cx=Vec(w*.5135f/h);
  Vec cy=(cx%cam.d).norm()*.5135f;
  Vec r;
  Vec *c=new Vec[w*h];

  constexpr int AA = 2;

  unsigned short Xi[3];
#pragma omp parallel
  {
    srand48(omp_get_thread_num());
    for (int s=0; s<samps; s++)
    {
      fprintf(stderr,"\rRendering (%d spp) : %d ",samps*4, s*4);

#pragma omp for schedule(guided)
      for (int y=0; y<h; y++)
        for (unsigned short x=0; x<w; x++)   // Loop cols
        {
          Vec r = Vec();
          const int idx = (h-y-1)*w+x;
          for (int sy=0; sy<AA; sy++)     // 2x2 subpixel rows
            for (int sx=0; sx<AA; sx++)
            {        // 2x2 subpixel cols
              double r1=2*drand48(), dx=r1<1 ? sqrt(r1)-1: 1-sqrt(2-r1);
              double r2=2*drand48(), dy=r2<1 ? sqrt(r2)-1: 1-sqrt(2-r2);
              Vec d = cx*( ( (sx+.5 + dx)/2 + x)/w - .5) +
                cy*( ( (sy+.5 + dy)/2 + y)/h - .5) + cam.d;
              r = r + radiance(Ray(cam.o+d*140,d.norm()),0,Xi); //*(1./samps);
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
