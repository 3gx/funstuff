template<class I, I... Is>
struct vector_c {};

template<class T, class Dim>
struct quantity
{
  T value;
};


using mass   = vector_c<int, 1,0,0,0,0,0,0>;
using length = vector_c<int, 0,1,0,0,0,0,0>;
using time   = vector_c<int, 0,0,1,0,0,0,0>;
using charge = vector_c<int, 0,0,0,1,0,0,0>;
using temp   = vector_c<int, 0,0,0,0,1,0,0>;
using intens = vector_c<int, 0,0,0,0,0,1,0>;
using angle  = vector_c<int, 0,0,0,0,0,0,1>;

template<class T, class D>
quantity<T,D>
operator+(quantity<T,D> x,quantity<T,D> y)
{
  return quantity<T,D>{x.value + y.value};
}


template<class T, class D>
quantity<T,D>
operator-(quantity<T,D> x,quantity<T,D> y)
{
  return quantity<T,D>{x.value - y.value};
}



template<class T1, class T2>
struct plus;

template<class T1, class T2>
using plus_t = typename plus<T1,T2>::type;

template<class I, I... I1s, I... I2s>
struct plus<vector_c<I,I1s...>,vector_c<I,I2s...>>
{
  using type = vector_c<I, (I1s+I2s)...>;
};

template<class T1, class T2>
struct minus;

template<class T1, class T2>
using minus_t = typename minus<T1,T2>::type;

template<class I, I... I1s, I... I2s>
struct minus<vector_c<I,I1s...>,vector_c<I,I2s...>>
{
  using type = vector_c<I, (I1s-I2s)...>;
};


template<class T, class D1, class D2>
quantity<T, plus_t<D1,D2> >
operator*(quantity<T,D1> x, quantity<T,D2> y)
{
  return quantity<T, plus_t<D1,D2>>{x.value*y.value};

}
template<class T, class D1, class D2>
quantity<T, minus_t<D1,D2> >
operator/(quantity<T,D1> x, quantity<T,D2> y)
{
  return quantity<T, minus_t<D1,D2>>{x.value/y.value};
}


using acceleration = vector_c<int,0,1,-2,0,0,0,0>;
using force        = vector_c<int,1,1,-2,0,0,0,0>;

int main()
{
  quantity<float,length> l{1.0f};
  quantity<float,mass>   m{2.0f};

  quantity<float, length> l1{1.0f};
  quantity<float, length> l2{2.0f};

  quantity<float, length> l3 = l1 + l2;

  quantity<float, acceleration>  g_earth{9.8f};
  quantity<float, acceleration>  g_mars{3.8f};

  quantity<float, force> f_earth = m*g_earth;
  quantity<float, mass>  m_mars = f_earth / g_mars;

  quantity<float, mass> m_earth = f_earth/g_earth;

  auto diff = m_earth - m;
  if (diff.value < 1.0e-7 && -diff.value < 1.0e-7)
    return 0;
  else
    return 1;



}
