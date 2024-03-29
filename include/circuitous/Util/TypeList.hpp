/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <type_traits>
#include <utility>

namespace circ::tl {

  template< typename ... Es >
  struct TL {};

  namespace detail {

    template< typename List > struct front_ {};
    template< typename List > struct pop_front_ {};
    template< typename H, typename List > struct push_front_ {};
    template< typename F, typename List > struct apply_ {};
    template< typename List > struct materialize_ {};
    template< typename L1, typename L2 > struct merge_ {};

    template< typename H, typename ...Tail >
    struct front_< TL< H, Tail ... > > {
      using type = H;
    };

    template< typename H, typename ...Tail >
    struct pop_front_< TL< H, Tail ... > > {
      using type = TL< Tail ... >;
    };

    template< typename H, typename ...Tail >
    struct push_front_< H, TL< Tail ... > > {
      using type = TL< H, Tail ... >;
    };

    template< typename ... Es >
    constexpr uint32_t size_( TL< Es ... > ) {
      return sizeof...( Es );
    }

    template< typename F, typename ... Es >
    struct apply_< F, TL< Es ... > >{
      using type = TL< typename F::template type< Es > ... >;
    };

    template< typename ... Es >
    struct materialize_< TL< Es ... > > : Es ... {};

    template< typename ...Rhs, typename ... Lhs >
    struct merge_< TL< Rhs... >, TL< Lhs ... > > {
      using type = TL< Rhs ..., Lhs ... >;
    };

    template< typename L, typename H, typename ...Tail >
    auto merge() {
      using current = typename merge_< L, H >::type;
      if constexpr ( sizeof...(Tail) == 0 ) {
        return current{};
      } else {
        return merge< current, Tail ... >();
      }
    }

    template< typename ... Ts > requires ( sizeof ... (Ts) == 0 )
    auto merge() { return tl::TL{}; }

    template< typename L >
    auto merge() { return L{}; }

    template< typename Acc, typename H, typename ... Tail >
    bool contains(Acc &&acc)
    {
      if (acc.template operator()< H >())
        return true;
      if constexpr (sizeof ... (Tail) != 0)
        return contains< Acc, Tail ... >(std::forward< Acc >(acc));
      else
        return false;
    }

    template< typename ... Es, typename Acc >
    bool contains(tl::TL< Es ... >, Acc &&acc)
    {
        if constexpr (sizeof ... (Es) == 0)
          return false;
        else
          return contains< Acc, Es ... >(std::forward< Acc >(acc));
    }

  } // namespace detail

  template< typename L > using front = typename detail::front_< L >::type;
  template< typename L > using pop_front = typename detail::pop_front_< L >::type;
  template< typename H, typename L >
  using push_front = typename detail::push_front_< H, L >::type;

  template< typename ... Ls >
  using merge = decltype( detail::merge< Ls ... >() );

  template< typename L, typename F >
  using apply = typename detail::apply_< F, L >::type;

  template< typename L > using materialized = detail::materialize_< L >;

  template<typename L>
  static constexpr uint32_t size = detail::size_( L{} );

  template< typename ...Ts >
  using make_list = TL< Ts ... >;

  template< typename L, typename Acc >
  bool contains(Acc &&acc) { return detail::contains(L{}, std::forward< Acc >(acc)); }

  namespace test {

    static_assert( std::is_same_v< front< TL< int, char * > >, int > );
    static_assert( std::is_same_v< front< TL< char * > >, char * > );

    static_assert( std::is_same_v< pop_front< TL< int, char * > >, TL< char * > > );
    static_assert( std::is_same_v< pop_front< TL< int > >, TL<> >);

    static_assert( std::is_same_v< push_front< int, TL<> >, TL< int > > );
    static_assert( std::is_same_v< push_front< int, TL< int, void > >, TL< int, int, void > > );

    static_assert( std::is_same_v< merge< TL<>, TL<> >, TL<> > );
    static_assert( std::is_same_v< merge< TL< int >, TL<> >, TL< int > > );
    static_assert( std::is_same_v< merge< TL< void >, TL< void > >, TL< void, void > > );

    static_assert( size< TL<> > == 0u );
    static_assert( size< TL< int > > == 1u );

    struct _mutate {
      template< typename T > using type = const T;
    };

    static_assert( std::is_same_v< apply< TL< int >, _mutate >, TL< const int > > );

  } // namespace test

} // namespace circ::tl
