#include <iostream>
#include <array>
#include <utility>

template <class... Ts>
constexpr auto sum_(Ts&&... args)
{
  return (args + ...);
}

template<typename T, size_t N, size_t... Is>
constexpr T sum_impl(std::array<T,N> const & arr, std::index_sequence<Is...>)
{
  return sum_(std::get<Is>(arr)...);
}

template<typename T, size_t N>
constexpr T sum(std::array<T,N> const &arr)
{
  return sum_impl(arr, std::make_index_sequence<N>{});
}

int main()
{
  constexpr std::array<int,4> arr =  {1,1,2,3};
  std::cout << std::integral_constant<int, sum(arr)>{} << std::endl;

}
