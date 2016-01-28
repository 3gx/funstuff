#include <iostream>
using namespace std;

template<class Child>
struct Base
{
  void interface()
  {
    static_cast<Child*>(this)->implementation();
  }
};

struct Derived : Base<Derived>
{
  void implementation()
  {
    cerr << "Derived implementation\n";
  }
};

int main()
{
  Derived d;
  d.interface();
}
