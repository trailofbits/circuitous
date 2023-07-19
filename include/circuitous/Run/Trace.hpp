/*
 * Copyright (c) 2021-present Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <fstream>

#include <circuitous/IR/Trace.hpp>

#include <circuitous/Run/State.hpp>

namespace circ::run::trace
{
    struct MaybeValue : std::optional< std::string >
    {
        using parent_t = std::optional< std::string >;
        using parent_t::parent_t;

        MaybeValue(std::optional< llvm::StringRef > w)
        {
            if (w)
                *this = w->str();
        }

        std::optional< llvm::APInt > cast(std::size_t size, std::size_t radix) const
        {
            if (!this->has_value())
                return {};
            return llvm::APInt(static_cast< uint32_t >(size),
                               **this,
                               static_cast< uint8_t >(radix));
        }
    };

    llvm::APInt cast( const std::string str, std::size_t radix )
    {
        auto value = MaybeValue( str ).cast( str.size(), radix );
        check( value ) << "Could not cast from \"" << str << "\" in radix" << radix
                       <<  "to APInt:" << str;
        return *value;
    }

    namespace native
    {

        struct Trace
        {
            using Entry = std::map< std::string, std::optional< llvm::APInt > >;
            using memory_t = std::unordered_map< uint64_t, value_type >;

            uint64_t id = 0;
            memory_t initial_memory = {};

            std::vector< Entry > entries;

            Trace( std::vector< Entry > entries )
                : entries( std::move( entries ) )
            {}

            Trace() = default;

            void push_back( Entry entry ) { entries.push_back( std::move( entry ) ); }

            std::string to_string() const;

            auto begin() { return entries.begin(); }
            auto begin() const { return entries.begin(); }
            auto end() { return entries.begin(); }
            auto end() const { return entries.begin(); }

            std::size_t size() const { return entries.size(); }
            auto &operator[](std::size_t idx) { return entries[idx]; }
            const auto &operator[](std::size_t idx) const { return entries[idx]; }
        };

        // Format:
        // {
        //  "id" = Integer
        //  "entries" : [
        //      {
        //          "timestamp" = Number as string in hex
        //          "error_flag" = Number as string in hex
        //          "instruction_bits" = Number as string in hex
        //          "regs" : [
        //              "RAX" = Number as string in hex
        //              ...
        //          ]
        //      }
        //  ]
        // }
        struct FromJSON
        {
            using self_t = FromJSON;
            using reg_sizes_map_t = std::unordered_map< std::string, std::size_t >;

            Trace trace;
            reg_sizes_map_t reg_sizes;

            template< typename O >
            static auto unwrap(const O &obj)
            {
                check(obj) << "Trying to unwrap invalid object!";
                return *obj;
            }

            template< typename O >
            static auto convert(const O &obj, std::size_t size, std::size_t radix)
            {
                return MaybeValue{obj}.cast(size, radix);
            }

            auto open_json(const std::string &path) const
            {
                // Open JSON
                auto maybe_buff = llvm::MemoryBuffer::getFile(path);
                check(maybe_buff) << "Error while opening JSON at: " << path;

                // Parse JSON
                auto maybe_json = llvm::json::parse(maybe_buff.get()->getBuffer());
                check(maybe_json) << "Error while parsing JSON at: " << path;

                auto out = maybe_json.get().getAsObject();
                check(out) << "Invalid loaded JSON object from: " << path;
                return *out;
            }

            auto take() { return std::move(trace); }

            Trace::memory_t parse_memory(const auto &obj)
            {
                Trace::memory_t out;
                for (const auto &[raw_addr, raw_val] : obj)
                {
                    auto maybe_addr = convert(raw_addr.str(), 64, 16);
                    check(maybe_addr);
                    auto addr = maybe_addr->getLimitedValue();
                    auto val = unwrap(raw_val.getAsString());

                    check(val.size() % 2 == 0);

                    for (std::size_t i = 0; i < val.size() / 2; ++i)
                        out[addr + i] = convert(val.substr(i * 2, 2), 8, 16);
                }
                return out;
            }

            self_t &run(const std::string &path)
            {
                auto obj = open_json(path);
                trace.id = static_cast< uint64_t >(unwrap(obj.getInteger("id")));
                if (auto maybe_initial_memory = obj.getObject("initial_memory"))
                    trace.initial_memory = parse_memory(unwrap(maybe_initial_memory));
                for (const auto &entry : unwrap(obj.getArray("entries")))
                {
                    auto x = ParseEntry().run(unwrap(entry.getAsObject())).take();
                    trace.entries.emplace_back(std::move(x));
                }
                return *this;

            }

            struct ParseEntry
            {
                using self_t = ParseEntry;

                Trace::Entry entry;

                auto convert( std::optional< llvm::StringRef > &&src )
                {
                    check( src );
                    auto size = src->size() * 4;
                    return FromJSON::convert(src, size, 16);
                }

                auto convert( std::optional< llvm::StringRef > &&src, std::size_t size )
                {
                    check( src );
                    return FromJSON::convert(src, size, 16);
                }

                self_t &run(const auto &obj)
                {
                    entry["timestamp"] = convert(obj.getString("timestamp"));
                    entry["error_flag"] = convert(obj.getString("error_flag"));
                    entry["instruction_bits"] = construct_inst_bits(
                            unwrap(obj.getString("instruction_bits")).str(), 15 * 8, 16);

                    for (const auto &[reg, val] : unwrap(obj.getObject("regs")))
                        entry[reg.str()] = convert(val.getAsString());

                    std::size_t idx = 0;
                    for (const auto &e : unwrap(obj.getArray("memory_hints")))
                    {
                        auto o = unwrap( e.getAsObject() );
                        std::vector< llvm::APInt > partials = {
                            llvm::APInt( 1, 1, false ),
                            ( *convert( o.getString( "mode" ), 64 ) ).trunc( 1 ),
                            llvm::APInt( 6, 0, false ),
                            ( *convert( o.getString( "id" ), 64 ) ).trunc( 4 ),
                            ( *convert( o.getString( "size" ), 64 ) ).trunc( 4 ),
                            *convert( o.getString( "addr" ), 64 ),
                            *convert( o.getString( "val" ), 64 ),
                            *convert( o.getString( "ts" ), 64 )
                        };

                        irops::memory::Parsed< llvm::APInt > parsed( 64,
                                                                     std::move( partials ) );

                        llvm::APInt out { irops::memory::size( 64 ), 0, false };
                        auto inserter = [ & ]( auto thing, auto from, auto size )
                        {
                            check( size == thing.getBitWidth() );
                            out.insertBits( thing, from );
                        };

                        irops::memory::construct( parsed, inserter );
                        entry[ "memory." + std::to_string( idx++ ) ] = out;
                    }
                    return *this;
                }

                llvm::APInt construct_inst_bits(
                        const std::string &str, std::size_t size, uint8_t radix)
                {
                    std::string reordered;

                    check(str.size() >= 2) << str.size() << " content:" << str;
                    for (int i = static_cast< int >(str.size() - 2); i >=0; i -=2)
                        reordered += str.substr(static_cast< unsigned long >(i), 2);
                    return llvm::APInt(static_cast< uint32_t >(size), reordered, radix);
                }

                Trace::Entry take() { return std::move(entry); }
            };
        };

        static inline auto load_json(const std::string &path)
        {
            return FromJSON().run(path).take();
        }

        static inline std::unordered_map< Operation *, value_type > make_step_trace(
                Circuit *circuit,
                const Trace::Entry &in,
                const Trace::Entry &out)
        {
            using VTrace = ValuedTrace< value_type >;
            auto input = VTrace(circ::Trace::make(circuit), in)
                .specialize(circuit, input_leaves_ts{});
            auto output = VTrace(circ::Trace::make(circuit), out)
                .specialize(circuit, output_leaves_ts{});

            for (const auto &[k, v] : output)
            {
                check(!input.count(k));
                input[k] = v;
            }

            for (auto &[k, v] : input)
            {
                // Coercion of sizes to perfectly fit registers is required (when loading,
                // some approximation is used to decouple loading code from Circuit itself).
                if (v)
                    v = std::make_optional(v->zextOrTrunc(k->size));
            }
            return input;
        }

    } // namespace native

    namespace mttn
    {

        // Returns string_view, therefore the original string must live long enough!
        struct lexer
        {
            // So we don't have to bother with lifetimes. This is not hot path
            // anyway.
            using value_type = std::string;

            value_type storage;
            std::size_t current = 0;

            auto eof() const
            {
                return current >= storage.size();
            }

            auto next( std::size_t size ) -> value_type
            {
                current += size;
                check( !eof() );
                return storage.substr( current - size, size );
            }

            static lexer from_string(const std::string &data)
            {
                return { data, 0 };
            }
        };

        struct trace_description
        {
            inline static const std::vector< std::string > gpr =
            {
                "EAX", "EBX", "ECX", "EDX",
                "ESI", "EDI",
                "ESP", "EBP",
                "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15"
            };

            inline static const std::vector< std::string > syscall_regs =
            {
                "EBX", "ECX", "EDX"
            };

            // Directly maps to cpu state
            struct reg
            {
                const std::size_t size = 32;
                const std::string name;

                reg( std::string name ) : name( name ) {}
            };

            struct memory_hint
            {
                static constexpr inline const std::size_t maximum = 2;
                // Operation mask, data + Addr (big endian) + Value (big endian)
                const std::size_t size = 8 + 32 + 32;
            };

            struct syscall_reg : protected reg
            {
                using reg::reg;
            };

            struct eflags
            {
                const std::size_t size = 4 * 8;
            };

            struct inst_bytes
            {
                // Padded with NOPs.
                const std::size_t size = 8 * 12;
            };

            using entry = std::variant< reg, memory_hint, syscall_reg, eflags, inst_bytes >;

            static gap::generator< entry > fields()
            {
                co_yield entry{ inst_bytes{} };

                for ( auto reg_name : gpr )
                    co_yield entry{ reg( reg_name ) };

                for ( auto reg_name : syscall_regs )
                    co_yield entry{ syscall_reg( reg_name ) };

                co_yield entry{ reg{ "EIP" } };
                co_yield entry{ eflags{} };

                for ( std::size_t i = 0; i < memory_hint::maximum; ++i )
                    co_yield entry{ memory_hint{} };
            }
        };

        template< typename L >
        struct parser : trace_description
        {
            using self_t = parser< L >;

            using lexer_type = L;
            using parse_map = std::map< std::string, typename lexer_type::value_type >;

            using entry = typename trace_description::entry;
          protected:

            const std::string &data;
            lexer_type lexer;

            parse_map parsed;

            parser( const std::string &data )
                : data( data ), lexer( L::from_string( data ) )
            {}

            void handle( auto && ) { log_kill() << "Not implemented."; }

            void handle( trace_description::reg reg )
            {
                auto value = lexer.next( reg.size );
                check( !parsed.count( reg.name ) );
                parsed[ reg.name ] = value;
            }

            void handle( trace_description::memory_hint hint )
            {

            }

            void handle( trace_description::eflags eflags )
            {
                auto token = lexer.next( eflags.size );
                auto at = [&]( auto idx ) { return token[ eflags.size - idx ]; };
                // 0 CF
                parsed[ "CF" ] = at( 0ul );
                // 2 PF
                parsed[ "PF" ] = at( 2ul );
                // 4 AF
                parsed[ "AF" ] = at( 4ul );
                // 6 ZF
                parsed[ "ZF" ] = at( 6ul );
                // 7 SF
                parsed[ "SF" ] = at( 7ul );
                // 11 OF
                parsed[ "DF" ] = at( 11ul );
                check( at( 1ul ) == '1' && at( 3ul ) == '0' && at( 5ul ) == '0' )
                    << "Sanity check of eflags encoding failed!";

            }

            void handle( trace_description::syscall_reg )
            {
                log_dbg() << "[run::trace::mttn]:" << "Ignoring syscall_regs for now!";
                return;
            }

            auto parse() &&
            {
                auto dispatch = [&](auto field) { return this->handle( field ); };

                for ( auto e : trace_description::fields() )
                    std::visit( dispatch, e );

                return std::move( parsed );
            }

          public:

            static parse_map parse( const std::string &data )
            {
                return parser< lexer_type >( data ).parse();
            }

            static auto as_native_entry( const std::string &data ) -> native::Trace::Entry
            {
                auto parse_map = parse( data );

                native::Trace::Entry out;
                for ( auto [ key, val ] : parse_map )
                    out[ key ] = cast( val, 2 );

                return out;
            }
        };


        auto load( const std::string &src, const ::circ::Trace &trace_fmt )
            -> native::Trace
        {
            std::ifstream input(src);
            check( input ) << "Problem opening file to load mttn trace from:" << src;

            native::Trace out;
            for ( std::string line; std::getline( input, line ); )
            {
                auto entry = parser< lexer >::as_native_entry( line );
                out.entries.emplace_back( std::move( entry ) );
            }

            return out;
        }

    } // namespace mttn

} // namespace circ::run::trace
