//# https://ngathanasiou.wordpress.com/author/ngathanasiou/
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

namespace fld
{

    template <class F, class T>
	struct XX
	{
		T data;

		template <class D>
		constexpr XX(D&& data)
			: data(std::forward<D>(data))
		{ }

		constexpr T give() const
		{
			return data;
		}
	};

	template <class L, class F, class R>
	struct YY
	{
		L l;
		R r;

		template <class LL, class RR>
		constexpr YY(LL&& l, RR&& r)
			: l(std::forward<LL>(l))
			, r(std::forward<RR>(r))
		{ }

		constexpr auto give() const
		{
			return F{}(l.give(), r.give());
		}
	};

	template <class F, class R, class T>
	constexpr auto operator+(XX<F, T> const& lhs, R const& rhs)
	{
		return YY< XX<F, T>, F, R >(lhs, rhs);
	}

	template <class F, class T1, class T2, class R>
	constexpr auto operator+(YY<T1, F, T2> const& lhs, R const& rhs)
	{
		return YY< YY<T1, F, T2>, F, R >(lhs, rhs);
	}

	namespace detail
	{
		template <class... Ts>
		constexpr auto _foldl(Ts&&... args)
		{
			return (args + ...);
		}

		template <class... Ts>
		constexpr auto _foldr(Ts&&... args)
		{
			return (... + args);
		}
	}

	template <class F, class... Ts>
	constexpr decltype(auto) foldl(Ts&&... args)
	{
		return detail::_foldl(XX<F, Ts>(args)...).give();
	}

	template <class F, class... Ts>
	constexpr decltype(auto) foldr(Ts&&... args)
	{
		return detail::_foldr(XX<F, Ts>(args)...).give();
	}

}

// 4. -------------------------------------------------------------------------
template <class T>
std::string Stringify(T const& value)
{
	std::stringstream ss;
	ss << value;
	return ss.str();
}

struct Join
{
	template <class T1, class T2>
	std::string operator()(T1 const& lhs, T2 const& rhs)
	{
		return Stringify(lhs) + Stringify(rhs);
	}
};

struct Vettore
{
	std::vector<int> operator()(int a, int b)
	{
		return{ a, b };
	}

	std::vector<int> operator()(int b, std::vector<int> const& a)
	{
		auto ret(a);
		ret.insert(ret.begin(), b);
		return ret;
	}
};

struct Max
{
	template <class T1, class T2>
	constexpr decltype(auto) operator()(T1&& lhs, T2&& rhs)
	{
		return lhs > rhs ? std::forward<T1>(lhs) : std::forward<T2>(rhs);
	}
};
// ~ --------------------------------------------------------------------------

int main()
{
	// 4. Use fold expressions for arbitrary operators ------------------------
	std::cout << fld::foldr<Join>(1, std::string(
		" bird in the hand, is worth "), 10, std::string(" in the bush")) << std::endl;

	auto k = fld::foldl<Vettore>(1, 2, 30, 12);
	for (auto&& i : k) std::cout << i << std::endl;

	static_assert(20 == fld::foldl<Max>(1, 20, 3, 5), "ET phone home");
	std::cout << "Reducing the maximum of [1, 20, 3, 5] : " 
		<< fld::foldl<Max>(1, 20, 3, 5) << std::endl;
}


