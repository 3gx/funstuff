#include <iostream>
#include <type_traits>

template<class T, class FROM, class TO>
struct replace_type
{
  using type = T;
};

template<class T, class FROM, class TO>
using replace_type_t = typename replace_type<T,FROM,TO>::type;



template<class T, class TO>
struct replace_type<T,T,TO>
{
  using type = TO;
};
template<class T, class TO>
struct replace_type<const T,T,TO>
{
  using type = const TO;
};
template<class T, class TO>
struct replace_type<T*,T ,TO>
{
  using type = TO*;
};
template<class T, class TO>
struct replace_type<T&,T ,TO>
{
  using type = TO&;
};
template<class T, class TO>
struct replace_type<T&&,T ,TO>
{
  using type = TO&&;
};
template<class T, class TO>
struct replace_type<T const&,T ,TO>
{
  using type = TO const&;
};
template<class T, class TO>
struct replace_type<T* const,T ,TO>
{
  using type = TO* const;
};
template<class T, class TO>
struct replace_type<T const*,T ,TO>
{
  using type = TO const*;
};
template<class T, class TO, size_t N>
struct replace_type<T const*[N],T ,TO>
{
  using type = TO const*[N];
};

template<class T, class TO,  class... Ds>
struct replace_type<T& (*)(Ds...), T, TO>
{
  using type = TO& (*)(replace_type_t<Ds,T,TO>...);
};


int main()
{
  using namespace std;

  cout << is_same< replace_type_t<char,char,long>, long>::value << endl;
  cout << is_same< replace_type_t<char&,char,long>, long&>::value << endl;
  cout << is_same< replace_type_t<char&&,char,long>, long&&>::value << endl;
  cout << is_same< replace_type_t<char const&,char,long>, long const&>::value << endl;
  cout << is_same< replace_type_t<char*,char,long>, long*>::value << endl;
  cout << is_same< replace_type_t<char* const,char,long>, long* const>::value << endl;
  cout << is_same< replace_type_t<int const*,int,long>, long const*>::value << endl;
  cout << is_same< replace_type_t<int const*[10],int,long>, long const*[10]>::value << endl;
  cout << is_same< replace_type_t<char& (*)(char&),char,long>, long& (*)(long&)>::value << endl;
  cout << is_same< replace_type_t<char& (*)(int&, float,char, double&, char*),char,long>, long& (*)(int&, float, long, double&, long*)>::value << endl;

};
