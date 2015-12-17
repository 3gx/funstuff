#include <array>
#include <tuple>
#include <string>
#include <vector>
#include <sstream>
#include <utility>
#include <iostream>
#include <type_traits>


namespace cpp17
{

    namespace detail 
	{
		// TODO: Write __invoke 
	} 

#if 0
	template <class F, class... Args>
	decltype(auto) invoke(F&& f, Args&&... args)
	{
		return detail::__invoke(std::forward<F>(f), std::forward<Args>(args)...);
	}
#endif // TODO: Activate the invoke interface

	namespace detail 
	{
		template <class F, class Tuple, std::size_t... I>
		constexpr decltype(auto) apply_impl(F&& f, Tuple&& t, std::index_sequence<I...>)
		{
#if 1
			return (std::forward<F>(f))(std::get<I>(std::forward<Tuple>(t))...);
#else
			return invoke(std::forward<F>(f), std::get<I>(std::forward<Tuple>(t))...);
#endif // TODO: Implement the second branch
		}
	} 

	template <class T>
	struct Valency
	{
		static constexpr std::size_t value = std::tuple_size<T>::value; 
	};

	template <class T, std::size_t I>
	struct Valency<std::array<T, I>>
	{
		static constexpr std::size_t value = I; 
	};

	template <class F, class C>
	constexpr decltype(auto) apply(F&& f, C&& t)
	{
		return detail::apply_impl(std::forward<F>(f), std::forward<C>(t),
			std::make_index_sequence< Valency<std::decay_t<C> >::value >{});
	}

}

// 2. -------------------------------------------------------------------------
namespace detail
{
	template <class... Ts>
	constexpr auto sum_(Ts&&... args)
	{
		return (args + ...);
	}

	template <typename T, std::size_t N, std::size_t... Is>
	constexpr T sum_impl(std::array<T, N> const &arr, std::index_sequence<Is...>)
	{
		return sum_(arr[Is]...);
	}
}

template <typename T, std::size_t N>
constexpr T sum(std::array<T, N> const &arr)
{
	return detail::sum_impl(arr, std::make_index_sequence<N>{});
}  
// 3. -------------------------------------------------------------------------
struct Summer
{
	template <class... Ts>
	constexpr auto operator()(Ts&&... args) { return (args + ...); }
};


int main()
{
	// 2. Sum the contents of an std::array at compile time -------------------
	constexpr std::array<int, 4> arr{ { 1, 1, 2, 3 } };
	std::cout << "Array sum : " << std::integral_constant<int, sum(arr)>{} << std::endl;
	
    // 3. Sum the contents of a tuple at compile time -------------------------
	constexpr std::tuple<int, int, int, int> tup{ 1, 1, 2, 3 }; 
	std::cout << "Tuple sum : " << std::integral_constant<int,
		cpp17::apply(Summer{}, tup)>{} << std::endl;
    
    // 3.b. Using a modification of apply to unfold std::arrays ---------------
    std::cout << "Array sum with apply : " << std::integral_constant<
        int, cpp17::apply(Summer{}, tup)>{} << std::endl;
}
