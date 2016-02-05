/* write unary metafunction add_const_Ref<T> that returns T if it si a ref type,
 * otherwise returns T const & */

#include <iostream>
#include <type_traits>

template<class T>
struct add_const_ref
{
  using type = T const&;
};


template<class T>
struct add_const_ref<T&>
{
  using type = T&;
};

template<class T>
struct is_const_ref : std::false_type {};

template<class T>
struct is_const_ref<const T&> : std::true_type{};

template<class T>
using add_const_ref_t = typename add_const_ref<T>::type;

template<class T>
class TD;

int main()
{
  using std::cout;
  using std::endl;


  cout << std::is_same<add_const_ref_t<int>,const int&>::value << endl;
  cout << std::is_same<add_const_ref_t<int&>,int&>::value << endl;
}
