 // Copyright 2020, Trail of Bits. All rights reserved.

#pragma once

#include <memory>

namespace circ
{
	template< typename T >
  struct constexpr_vector
	{
		using value_type 		  = T;
		using reference 		  = value_type&;
		using const_reference = const value_type&;
		using pointer 				= value_type*;
		using const_pointer 	= const value_type*;
		using iterator 			  = pointer;
		using const_iterator  = const_pointer;

		constexpr constexpr_vector() = default;

		explicit constexpr constexpr_vector(std::size_t n)
			: _size(0), _capacity(n), _data( new T[_capacity] )
		{
			for (std::size_t i  = 0; i < n; ++i)
				_data[i] = T{};
		}

		template< typename It >
		constexpr constexpr_vector(It begin, It end)
			: constexpr_vector(std::size_t( std::distance(begin, end) ))
		{
			_size = std::size_t( std::distance(begin, end) );
			std::copy(begin, end, _data); // unitialized?
		}

		constexpr constexpr_vector(const constexpr_vector &other)
			: constexpr_vector(other.begin(), other.end())
		{}

		constexpr constexpr_vector(constexpr_vector &&other)
			: constexpr_vector()
		{
			swap(other);
		}

		constexpr constexpr_vector(std::initializer_list<T> list)
			: constexpr_vector(list.begin(), list.end())
		{}

		constexpr constexpr_vector& operator=(constexpr_vector other)
		{
			swap(other);
			return *this;
		}

		constexpr ~constexpr_vector()
		{
			if (_data)
				delete[] _data;
		}

		constexpr auto capacity() const { return _capacity; }
    constexpr auto size()  const { return _size; }
    constexpr auto empty() const { return _size == 0; }

		constexpr iterator begin() { return _data; }
		constexpr const_iterator begin() const { return _data; }
		constexpr const_iterator cbegin() const { return _data; }

		constexpr iterator end() { return std::next( _data, long(_size)); }
		constexpr const_iterator end() const { return std::next(_data, long(_size)); }
		constexpr const_iterator cend() const { return std::next(_data, long(_size)); }

		constexpr reference operator[](const std::size_t pos) { return _data[pos]; }
		constexpr const_reference operator[](const std::size_t pos) const { return _data[pos]; }

		constexpr reference at(const std::size_t pos)
		{
			if (pos >= _size) {
				throw std::range_error("access out of bounds");
			} else {
				return _data[pos];
			}
		}

		constexpr const_reference at(const std::size_t pos) const
		{
			if (pos >= _size) {
				throw std::range_error("access out of bounds");
			} else {
				return _data[pos];
			}
		}

		constexpr reference front()
		{
			if (empty()) {
				throw std::range_error("accessing empty vector");
			} else {
				return _data[0];
			}
		}

		constexpr const_reference front() const
		{
			if (empty()) {
				throw std::range_error("accessing empty vector");
			} else {
				return _data[0];
			}
		}

		constexpr reference back()
		{
			if (empty()) {
				throw std::range_error("accessing empty vector");
			} else {
				return _data[_size - 1];
			}
		}

		constexpr const_reference back() const
		{
			if (empty()) {
				throw std::range_error("accessing empty vector");
			} else {
				return _data[_size - 1];
			}
		}

		constexpr reference push_back(const T &value)
		{
			if (_size >= _capacity) {
				throw std::range_error("overfill of constexpr vector");
			} else {
				_data[_size++] = value;
				return back();
			}
		}

		constexpr reference push_back(T &&value)
		{
			if (_size >= _capacity) {
				throw std::range_error("overfill of constexpr vector");
			} else {
				_data[_size++] = std::move(value);
				return back();
			}
		}

		template< typename ...Args >
		constexpr reference emplace_back( Args&& ... args )
		{
			if (_size >= _capacity) {
				throw std::range_error("overfill of constexpr vector");
			} else {
				_data[_size++] = T( std::forward<Args>(args)... );
				return back();
			}
		}

    constexpr void clear() { _size = 0; }

    constexpr const_pointer data() const { return _data; }

		constexpr void swap(constexpr_vector &other) noexcept
		{
			using std::swap;

			swap(_size, other._size);
			swap(_capacity, other._capacity);
			swap(_data, other._data);
		}

		constexpr friend void swap(constexpr_vector &lhs, constexpr_vector &rhs)
		{
			lhs.swap(rhs);
		}

	private:
		std::size_t _size{0};
		std::size_t _capacity{0};
		T *_data = nullptr;
	};

	template< typename T >
	constexpr bool operator==(const constexpr_vector<T> &lhs, const constexpr_vector<T> &rhs)
	{
		return std::equal(lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
	}

	template< typename T >
	constexpr auto operator+(const constexpr_vector<T> &lhs, const constexpr_vector<T> &rhs)
	{
		constexpr_vector<T> result(lhs.size() + rhs.size());
		std::copy(lhs.begin(), lhs.end(), std::back_inserter(result));
		std::copy(rhs.begin(), rhs.end(), std::back_inserter(result));
		return result;
	}

} // namespace circuitous