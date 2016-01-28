#include <iostream>
#include <string>
#include <cassert>
using namespace std;

template<class Derived>
struct Comparisons
{
};

template<class Derived>
bool operator==(const Comparisons<Derived>& lhs, const Comparisons<Derived> &rhs)
{
  const auto d1 = static_cast<const Derived&>(lhs);
  const auto d2 = static_cast<const Derived&>(rhs);
  return !(d1 < d2) && !(d2 < d1);
}

template<class Derived>
bool operator!=(const Comparisons<Derived>& lhs, const Comparisons<Derived> &rhs)
{
  return !(lhs == rhs);
}

class Person : public Comparisons<Person>
{
  public:
    Person(string name, int age) :  name(name), age(age) {}

    friend bool operator<(const Person&, const Person&);

private:
    string name;
    int age;
};

bool operator<(const Person& lhs, const Person& rhs)
{
  return lhs.age < rhs.age;
}

int main(int argc, char *argv[]) 
{
  auto p1 = Person("may", 31);
  auto p2 = Person("may", 33);
  auto p3 = Person("jay", 31);
  auto p4 = Person("jay", 33);

  cerr << " p1==p2: " << (p1 == p2) << endl;
  cerr << " p1!=p2: " << (p1 != p2) << endl;
  cerr << " p1==p3: " << (p1 == p3) << endl;
  cerr << " p1!=p3: " << (p1 != p3) << endl;
}

