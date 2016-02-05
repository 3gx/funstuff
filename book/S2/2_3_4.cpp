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

template<class T>
struct type_descriptor
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_to_string<T>::value;
    return os;
  }
};

template<class T>
struct type_descriptor<T const>
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_descriptor<T>() << " const";
    return os;
  }
};

template<class T>
struct type_descriptor<T *>
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_descriptor<T>() << "*";
    return os;
  }
};
template<class T>
struct type_descriptor<T &>
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_descriptor<T>() << "&";
    return os;
  }
};
template<class T>
struct type_descriptor<T &&>
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_descriptor<T>() << "&&";
    return os;
  }
};

template<class T>
struct type_descriptor<T volatile>
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_descriptor<T>() << " volatile";
    return os;
  }
};


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
//  cout << type_descriptor<long const* volatile>() << endl;
}
