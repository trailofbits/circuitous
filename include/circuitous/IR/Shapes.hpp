/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Support/Check.hpp>

#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ
{
    // It is expected `accept` will be copied.
    template< typename Accept >
    std::vector< Operation * > topology( Operation *root, Accept accept )
    {
        std::vector< Operation * > out;
        std::unordered_set< Operation * > seen;
        topology( root, out, seen, accept );
        return out;
    }

    template< typename Accept >
    void topology( Operation *root, std::vector< Operation * > &collected,
                                    std::unordered_set< Operation * > &seen,
                                    Accept accept )
    {
        if ( seen.count( root ) )
            return;
        seen.emplace( root );

        for ( auto op : root->operands() )
            topology( op, collected, seen, accept );

        if ( accept( root ) )
            collected.push_back( root );
    }

    namespace print
    {
        template< typename Derived >
        struct Topology
        {
            std::stringstream ss;
            using hash_t = std::string;

            Derived &Self() { return static_cast< Derived & >( *this ); }

            template< typename C >
            std::string Hash( const C& ops )
            {
                std::stringstream hash;
                for ( auto op : ops )
                    hash << Hash(op) << " | ";

                return hash.str();
            }

            std::string Hash( Operation *op )
            {
                return Self().Print( op, 0 );
            }

            std::string Children( Operation *op, uint8_t depth )
            {
                std::stringstream out;
                for ( auto o : op->operands() )
                    out << Self().Print(o, depth + 1) << Self().separator;
                return out.str();
            }

            std::string Print( Operation *op )
            {
              return Self().Print( op, 0 );
            }

            std::string Print( Operation *op, uint8_t depth )
            {
                auto indent = Self().Indent( depth );
                std::stringstream out;
                out << indent
                    << Self().Op( op )
                    << "( "
                    << Self().Children( op, depth )
                    << indent + ")";
                return out.str();
            }

            std::string Get() { return ss.str(); }
            std::string Indent( uint8_t ) { return {}; }
        };

        template< typename Next >
        struct WithCache : Next
        {
            using parent_t = Next;
            using hash_t = typename parent_t::hash_t;

            using Next::Hash;

            std::unordered_map< Operation *, hash_t > op_to_hash;

            std::string Print( Operation *op, uint8_t depth )
            {
                auto it = op_to_hash.find( op );
                if ( it != op_to_hash.end() )
                    return it->second;

                auto x = this->parent_t::Print( op, depth + 1 );
                op_to_hash[ op ] = x;
                return x;
            }
        };

        template< typename Next >
        struct FullNames_ : Next
        {
            static inline constexpr const char separator = ' ';
            std::string Op( Operation *op ) { return op->name(); }
        };

        struct FullNames : FullNames_< WithCache< Topology< FullNames > > > {};

        struct PrettyPrinter : FullNames
        {
            std::string Indent( uint8_t depth ) { return std::string(depth * 2, ' '); }
        };

    } // namespace print



    namespace collect
    {
        struct Ctxs
        {
            using ctxs_t = std::unordered_set< Operation * >;
            using ctxs_map_t = std::unordered_map< Operation *, ctxs_t >;

            ctxs_map_t op_to_ctxs;

            void Root( Operation *op )
            {
                op_to_ctxs[ op ] = { op };
            }

            void Update( Operation *node, Operation *user )
            {
                if ( !user )
                    return;
              auto &ctxs = op_to_ctxs[ node ];
              auto &user_ctxs = op_to_ctxs[ user ];
              ctxs.insert( user_ctxs.begin(), user_ctxs.end() );
            }
        };

        struct Hashes : print::FullNames
        {
            void Root( Operation *op ) { Hash( op ); }

            void Update( Operation *node, Operation *user )
            {
                check(op_to_hash.count( node ) );
            }
        };

        struct AllowsUndef
        {
            std::optional< bool > allows;

            void Root(Operation *op) {}

            void Update( Operation *node, Operation *user )
            {
                if ( node->op_code == Undefined::kind )
                    allows = true;
            }
        };

        // TODO(lukas): Rename and rework using generators.
        template< typename Self >
        struct TreeCollector_
        {
            std::unordered_set< Operation * > collected;

            auto take() { return std::move( collected ); }
            Self &self() { return static_cast< Self & >( *this ); }

            Self &run( Operation *op )
            {
                if ( self().accepted( op ) )
                    collected.insert( op );

                for ( auto o : self().next( op ) )
                    run( o );
                return self();
            }

            template< typename T >
            gap::generator< T * > peek()
            {
                for ( auto op : collected )
                    if ( isa< T >( op ) )
                        co_yield static_cast< T * >( op );
            }

            template< typename T, typename C = std::unordered_set< T * > >
            C freeze_as()
            {
                C out;
                // There is a different type as sentinel.
                for ( auto x : peek< T >() )
                    out.emplace( x );
                return out;
            }
        };

        template< typename ... Ts >
        struct MatchOn
        {
            bool accepted( Operation *op ) { return is_one_of< Ts ... >( op ); }
        };

        struct Up
        {
            auto next( Operation *op ) { return op->users(); }
        };

        struct Down
        {
            auto next( Operation *op ) { return op->operands(); }
        };

        template< typename Dir >
        struct Unique : Dir
        {
            std::unordered_set< Operation * > seen;

            gap::generator< Operation * > next( Operation *op )
            {
                // TODO(lukas): Use generic filter.
                for ( auto x : this->Dir::next( op ) )
                {
                    if ( seen.count( x ) )
                        continue;
                    seen.insert( x );
                    co_yield x;
                }
            }
        };


        template< typename Dir, typename ... Ts >
        struct TreeCollector : TreeCollector_< TreeCollector< Dir, Ts ... > >,
                               MatchOn< Ts ... >,
                               Dir
        {};

        template< typename ... Ts >
        using UpTree = TreeCollector< Up, Ts ... >;

        template< typename ... Ts >
        using DownTree = TreeCollector< Down, Ts ... >;

    } // namespace collect

    template< typename ... Ts >
    gap::generator< Operation * > all_parents_of_types( Operation *op )
    {
        // TODO(lukas): Recursive generator.
        using namespace collect;
        for ( auto x : TreeCollector< Unique< Up >, Ts ... >().run( op ).take() )
            co_yield x;
    }

    template< typename ... Ts >
    auto all_parents_of_types( tl::TL< Ts ... >, Operation *op )
    {
        return all_parents_of_types< Ts ... >( op );
    }

    static inline bool is_in_decoder_subtree( Operation *op )
    {
        for ( auto _ : all_parents_of_types< DecoderResult >( op ) )
        {
            // To avoid warning about unused variable.
            std::ignore = _;
            return true;
        }
        return false;
    }


    static inline std::unordered_set< VerifyInstruction * > get_contexts( Operation *op )
    {
        std::unordered_set< VerifyInstruction * > out;
        for ( auto x : all_parents_of_types< VerifyInstruction >( op ) )
        {
            auto casted = dyn_cast< VerifyInstruction >( x );
            check( x );
            out.emplace( casted );
        }
        return out;
    }

    static inline std::optional< VerifyInstruction * > get_context( Operation *op )
    {
        auto all = get_contexts( op );
        if ( all.size() != 1 )
            return {};
        return { *all.begin() };
    }

    // TODO(lukas): Generalise walking mechanism.
    struct CtxCollector
    {
        using ctxs_t = std::unordered_set< VerifyInstruction * >;
        using ctxs_map_t = std::unordered_map< Operation *, ctxs_t >;

        Circuit *circuit;
        ctxs_map_t ctx_map;

      protected:

        struct Runner
        {
          private:
            std::unordered_map< Operation *, std::size_t > blocked;
            std::deque< Operation * > todo;
            ctxs_map_t ctx_map;
            Circuit *circuit;

            Runner( Circuit *circuit_ )
                : circuit( circuit_ )
            {
                for ( auto ctx : circuit->attr< VerifyInstruction >() )
                {
                    ctx_map[ ctx ].emplace( ctx );
                    signal_unblocked( ctx );
                }

                while ( !todo.empty() )
                {
                    auto x = todo.front();
                    todo.pop_front();
                    process( x );
                }
            }

            void process( Operation *op )
            {
                auto &ctxs = ctx_map[ op ];
                for ( auto user : op->users() )
                {
                    auto &user_ctxs = ctx_map[ user ];
                    ctxs.insert( user_ctxs.begin(), user_ctxs.end() );
                }
                signal_unblocked( op );
            }

            void mk_blocked( Operation *op )
            {
                blocked[ op ] = op->users_size();
            }

            void mk_todo( Operation *op ) { todo.push_back( op ); }

            void signal_unblocked( Operation *user )
            {
                std::unordered_set< Operation * > uniques;
                for ( auto op : user->operands() )
                    uniques.insert( op );
                for ( auto op : uniques )
                {
                    if ( !blocked.count( op ) )
                        mk_blocked( op );

                    auto &blocking = blocked[ op ];
                    check( blocking >= 1 ) << "Blocking is" << blocking
                                           << "which may signal there is a cycle in the IR.";
                    --blocking;
                    if ( blocking == 0 )
                        mk_todo( op );
                }
            }

          public:

            static ctxs_map_t run( Circuit *circuit )
            {
                return std::move( Runner( circuit ).ctx_map );
            }
        };

      public:

        CtxCollector( Circuit *circuit )
            : circuit( circuit ), ctx_map( Runner::run( circuit ) )
        {}

        CtxCollector( CtxCollector && ) = default;
        CtxCollector( const CtxCollector & ) = default;

        CtxCollector &operator=( CtxCollector && ) = default;
        CtxCollector &operator=( const CtxCollector & ) = default;

        const ctxs_t &operator[]( Operation *op ) const
        {
            auto it = ctx_map.find( op );
            check( it != ctx_map.end() );
            return it->second;
        }

        bool is_in_ctx( Operation *op, Operation *ctx ) const
        {
            auto casted = dyn_cast< VerifyInstruction >( ctx );
            return casted && is_in_ctx( op, casted );
        }

        bool is_in_ctx( Operation *op, VerifyInstruction *ctx ) const
        {
            auto it = ctx_map.find( op );
            if ( it == ctx_map.end() )
                return false;
            return it->second.count( ctx );
        }
    };

    static inline bool allows_undef_( Operation *op, std::unordered_set< Operation * > &seen )
    {
        if ( seen.count( op ) )
            return false;
        seen.insert( op );

        if ( op->op_code == Undefined::kind )
            return true;

        for ( auto x : op->operands() )
            if ( allows_undef_( x, seen ) )
                return true;
        return false;
    }

    static inline bool allows_undef( Operation *op )
    {
        if ( op->op_code != RegConstraint::kind ||
             op->operand( 1 )->op_code != OutputRegister::kind )
        {
            return false;
        }
        std::unordered_set< Operation * > seen;
        return allows_undef_( op, seen );
    }


    /*
     *  Finds sub-trees in a DFS starting at some node
     *  it returns a collection of all paths that start with top and end at bottom
     */
    template < typename Derived, bool IsConst = false >
    struct SubPathCollector : BacktrackingPathVisitor<Derived, IsConst>
    {
        using parent_t = BacktrackingPathVisitor< Derived, IsConst >;
        using operation_t = typename parent_t::operation_t;
        using path_t = typename parent_t::path_t;

        std::vector<path_t> collected;

        bool top(Operation *op) { return static_cast<Derived &>(*this).top( op );}
        bool bottom(Operation *op) { return static_cast<Derived &>(*this).bottom( op );}

        // users can override this, so they can reverse the direction of traversal
        void visit(circ::Operation *op) { op->traverse(*this); }

        // By keeping this logic located inside dispatch we allow the user
        // which direction to traverse
        auto dispatch(operation_t op)
        {
            /*
             * once we have reached the bottom, we recurse back to the original starting node
             * and saving a path for from bottom to any node satisfying top
             */
            if( bottom( op ) )
                collect_until_top( op );

            return this->parent_t::dispatch( op );
        }

        void collect_until_top( const operation_t &op )
        {
            path_t path_to_save;
            for ( auto it = this->current_path.rbegin(); it != this->current_path.rend(); ++it )
            {
                path_to_save.emplace_back( *it );
                if ( top( *it ) )
                {
                    // op hasn't been added to the path just yet.
                    path_to_save.emplace(path_to_save.begin(), op );
                    // We want this explicit copy.
                    collected.push_back( path_to_save );
                }
            }
        }

        /*
         * The intended way of calling.
         * Clears out the old collection and starts visiting from the provided op.
         */
        std::vector<std::vector< Operation * > > operator()( Operation *op )
        {
            this->collected.clear();
            /*
             * BacktrackingPathVisitor only adds nodes during dispatch
             * which only will be called for the children of op.
             */
            this->current_path.push_back(op);
            visit( op );
            return this->collected;
        }
    };

    template< typename TL, typename Visitor >
    void run_visitor_on(Operation *op, Visitor &&vis)
    {
        collect::DownTree <TL> down_collector; // Works on more than just circuit unlike attr
        down_collector.run( op );
        for (auto &o: down_collector.take()) {
            vis.visit( o );
        }
    }
} // namespace circ
