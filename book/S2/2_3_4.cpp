#include <iostream>

template<class T>
struct type_to_string;

template<>
struct type_to_string<char>
{
  static constexpr const char *get(){return "char";};
};
template<>
struct type_to_string<short int>
{
  static constexpr const char *get(){return "short int";};
};
template<>
struct type_to_string<int>
{
  static constexpr const char *get(){return "int";};
};
template<>
struct type_to_string<long int>
{
  static constexpr const char *get(){return "long int";}
};


struct Const 
{
  static constexpr const char* get() 
  {
    return " const";
  }
};
struct Volatile 
{
  static constexpr const char* get() 
  {
    return " volatile";
  }
};
struct Pointer
{
  static constexpr const char* get() 
  {
    return "*";
  }
};
struct LValRef
{
  static constexpr const char* get() 
  {
    return "&";
  }
};
struct RValRef
{
  static constexpr const char* get() 
  {
    return "&&";
  }
};

template<char... Cs>
struct metastring {};

template<char... Cs>
std::ostream& operator<<(std::ostream& os, metastring<Cs...>)
{
  const char data[sizeof...(Cs)] = {Cs...};
  os << data;
  return os;
}

template<size_t N, char... Cs>
struct number_string
{
  using type = typename number_string<N/10, '0'+(N%10), Cs...>::type;
};
template<char... Cs>
struct number_string<0,Cs...>
{
  using type = metastring<'*','[',Cs...,']'>;
};



template<size_t N>
struct Array
{
  using metastring = typename number_string<N>::type;
  static const metastring get() 
  {
    return metastring{};
  }
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
    os << Qual::get()<< QualPrinter<Quals...>();
    return os;
  }
};


template<class Type, class... Quals>
struct type_descriptor
{
  friend std::ostream& operator<<(std::ostream& os, const type_descriptor&)
  {
    os << type_to_string<Type>::get() << QualPrinter<Quals...>();
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
template <class T, size_t N, class... Quals>
struct type_descriptor<T *[N], Quals...> : type_descriptor<T, Array<N>, Quals...> {};


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
  cout << type_descriptor<long const*[29]>() << endl;
}
