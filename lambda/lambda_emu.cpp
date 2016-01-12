#include <iostream>
using namespace std;

template<class F>
void for_each(int n, F f)
{
  for (int i = 0; i < n; i++)
    f(i);
}

template <class L, class Ret = void> struct Lambda
{
  L l;
  template <class... Args> Ret operator()(Args &&... args)
  {
    l(std::forward<Args>(args)...);
  }
};

template <class L> Lambda<L> wrap_lambda(L l) { return Lambda<L>{l}; }

int main(int argc, char*argv[])
{
  int n = 12*argc;

  for_each(n,wrap_lambda([](int i){printf("%d \n",i);}));
}
