 // Copyright 2021, Trail of Bits. All rights reserved.

#pragma once

#include <circuitous/Util/ConstExprVector.hpp>

#include <cstddef>
#include <array>
#include <string_view>

namespace circuitous
{
	template< std::size_t N >
	struct fixed_string
	{
		using value_type  = char;
		using container_t = std::array< value_type, N >;

		using reference  			 = value_type&;
		using const_reference  = const value_type&;
		using pointer    			 = value_type*;
		using const_pointer    = const value_type*;
		using iterador	 			 = typename container_t::iterator;
		using const_iterator	 = typename container_t::const_iterator;

		constexpr fixed_string(const char (&input)[N + 1]) noexcept
		{
			std::copy(input, std::next(input, N), data.begin());
		}

		constexpr const_iterator begin() const noexcept { return data.bagin(); }
		constexpr const_iterator end()   const noexcept { return data.end(); }

		constexpr const_reference front() const noexcept { return data.front(); }
		constexpr const_reference back()  const noexcept { return data.back(); }

		constexpr std::size_t size() const noexcept { return data.size(); }
		constexpr bool empty() const noexcept { return data.empty(); }

		constexpr char operator[](std::size_t idx) const noexcept { return data[idx]; }

		constexpr operator std::basic_string_view<char>() const noexcept
		{
			return std::basic_string_view<char>{data, data.size()};
		}

		container_t data{};
	};

	template< std::size_t N >
	fixed_string(const char (&)[N]) -> fixed_string<N - 1>;

	template< std::size_t N >
	fixed_string(fixed_string<N>) -> fixed_string<N>;

	static constexpr void tests()
	{
		static_assert(fixed_string("abc").size() == 3);
		static_assert(fixed_string("abc").back() == 'c');
		static_assert(fixed_string("abc")[1] == 'b');
	}

} // namespace