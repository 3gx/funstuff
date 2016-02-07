template<int N> struct arg;

struct void_;

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


template<class, class...>
struct apply;

template<template<class...> class T, class... Ps, class... Ts>
struct apply<T<Ps...>,Ts...>
{
  using type = T<typename Ps::template apply<Ts...>::type...>;
};

template<class T>
class TD;

int main()
{
  using type_int = typename apply<mytype<_3,_1>, int, float,char>::type;
  TD<type_int> td;
}
