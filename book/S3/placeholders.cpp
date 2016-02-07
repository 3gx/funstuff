template<int N> struct arg;

template<>
struct arg<1>
{
  template<class A1, class...>
  struct apply
  {
    using type = A1;
  };
};
using _1 = arg<1>;

template<>
struct arg<2>
{
  template<class A1, class A2, class...>
  struct apply
  {
    using type = A2;
  };
};
using _2 = arg<2>;

template<>
struct arg<3>
{
  template<class A1, class A2, class A3, class...>
  struct apply
  {
    using type = A3;
  };
};
using _3 = arg<3>;

using _ = arg<-1>;

template<class...>
struct mytype;

template<class T>
struct mytype<T>
{
  T value;
};

template<class T, class U>
struct mytype<T,U>
{
  T valueT;
  U valueU;
};


template<class P, class... Ts>
struct apply
{
  using type = typename P::template apply<Ts...>::type;
};

template<class T, class... Ts>
using apply_t = typename apply<T,Ts...>::type;

template<template<class...> class T, class... Ps, class... Ts>
struct apply<T<Ps...>,Ts...>
{
  using type = T<apply_t<Ps, Ts...>...>;
};

template<class T>
class TD;

template<class A, class B>
struct AB
{
  A a;
  B b;
};

template<class T>
struct vector
{
  T data[32];
};

int main()
{
  using type_int = apply_t<mytype<_3,_1>, int, float,char>;
//  TD<type_int> td;
//
  using type_AB = AB<mytype<_3>, mytype<_2,_1>>;
  using type_abc = apply_t<type_AB, int,float,char>;
//  TD<type_abc> td;

  using type_1 = apply_t<_1, vector<_1>>;
  using vec_int = apply_t<type_1,int>;
//  TD<vec_int> td;

  using type_mytype_1 = apply_t<mytype<_1>, vector<_1>>;
  using mytype_vec_int = apply_t<type_mytype_1, int>;
//  TD<mytype_vec_int> td;

}
