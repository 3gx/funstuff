#include <iostream>

template<class T>
struct type_to_string;

template<>
struct type_to_string<char>
{
  static constexpr auto  value = "char";
};
template<>
struct type_to_string<short int>
{
  static constexpr auto  value = "short int";
};
template<>
struct type_to_string<int>
{
  static constexpr auto  value = "int";
};
template<>
struct type_to_string<long int>
{
  static constexpr auto  value = "long int";
};


struct Const 
{
  static constexpr auto value = " const";
};
struct Volatile 
{
  static constexpr auto value = " volatile";
};
struct Pointer
{
  static constexpr auto value = "*";
};
struct LValRef
{
  static constexpr auto value = "&";
};
struct RValRef
{
  static constexpr auto value = "&&";
};


template<class...>
struct QualPrinter
{
  friend void operator<<(std::ostream &os, const QualPrinter &) {}
};

template<class Qual, class... Quals>
struct QualPrinter<Qual, Quals...>
{
  friend std::ostream& operator<<(std::ostream& os, const QualPrinter&)
  {
    os << Qual::value << QualPrinter<Quals...>();
    return os;
  }
};


template<class Type, class... Quals>
struct type_descriptor
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_to_string<Type>::value << QualPrinter<Quals...>();
    return os;
  }
};

template <class T, class... Quals>
struct type_descriptor<T const, Quals...> : type_descriptor<T, Const, Quals...> {};

template <class T, class... Quals>
struct type_descriptor<T volatile, Quals...> : type_descriptor<T, Volatile, Quals...> {};

template <class T, class... Quals>
struct type_descriptor<T *, Quals...> : type_descriptor<T, Pointer, Quals...> {};

template <class T, class... Quals>
struct type_descriptor<T &, Quals...> : type_descriptor<T, LValRef, Quals...> {};

template <class T, class... Quals>
struct type_descriptor<T &&, Quals...> : type_descriptor<T, RValRef, Quals...> {};


int main()
{
  using namespace std;

  cout << type_descriptor<int>() << endl;
  cout << type_descriptor<long int>() << endl;
  cout << type_descriptor<char >() << endl;
  cout << type_descriptor<short >() << endl;
  cout << type_descriptor<int const*& >() << endl;
  cout << type_descriptor<int const*&& >() << endl;
  cout << type_descriptor<int const* volatile >() << endl;
  cout << type_descriptor<int * volatile >() << endl;
  cout << type_descriptor<long const* volatile>() << endl;
}
