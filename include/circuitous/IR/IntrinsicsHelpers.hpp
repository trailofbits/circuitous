/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>

#include <circuitous/Util/TypeList.hpp>

#include <vector>
#include <span>

namespace circ::irops::impl
{

    // TODO(lukas): Remove once we have `std::span`.
    static inline std::optional< uint32_t > uniform_size(auto begin, auto end)
    {
        check(begin != end, [](){ return "Trying to get uniform size from nothing."; });
        std::optional< uint32_t > size;
        for (auto what : llvm::iterator_range(begin, end)) {
            auto what_size = what->getType()->getIntegerBitWidth();
            if (size && what_size != size)
                return std::nullopt;
            if (!size)
                size = std::make_optional(what_size);
        }
        return size;
    }


    // Add size of each `llvm::Type` from `c_args` together.
    uint32_t sum_sizes(auto &&begin, auto &&end)
    {
        uint32_t acc = 0;
        while (begin != end)
        {
            auto int_ty = llvm::cast<llvm::IntegerType>((*begin)->getType());
            acc += int_ty->getBitWidth();
            ++begin;
        }
        return acc;
    }

    enum io_type : uint32_t
    {
        in = 0,
        out = 1,
    };

    static inline std::string to_string(io_type io)
    {
        switch(io)
        {
            case io_type::in : return "in";
            case io_type::out : return "out";
        }
    }


    template< typename E >
    struct to_enum {};

    template<>
    struct to_enum< io_type >
    {
        static io_type convert(std::string str)
        {
            if (str == to_string(io_type::in))
                return io_type::in;
            if (str == to_string(io_type::out))
                return io_type::out;

            log_kill() << "Cannot convert" << str << "to io_type enum";
        }
    };

    static llvm::Function* freeze(llvm::Function *fn)
    {
        fn->addFnAttr(llvm::Attribute::NoMerge);
        fn->addFnAttr(llvm::Attribute::OptimizeNone);
        fn->addFnAttr(llvm::Attribute::NoInline);
        fn->removeFnAttr(llvm::Attribute::ReadNone);
        fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
        return fn;
    }

    static llvm::Function* melt(llvm::Function *fn)
    {
        fn->removeFnAttr(llvm::Attribute::NoMerge);
        fn->removeFnAttr(llvm::Attribute::OptimizeNone);
        fn->removeFnAttr(llvm::Attribute::NoInline);
        fn->addFnAttr(llvm::Attribute::ReadNone);
        fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
        return fn;
    }

    namespace suffix
    {
        static inline uint64_t to_int(llvm::StringRef str_ref)
        {
            uint64_t out;
            str_ref.getAsInteger(10, out);
            return out;
        }

        template< typename I >
        struct i
        {
            using parsed_t = I;
            using input_t = parsed_t;

            static std::string make(I i) { return std::to_string(i); }
            static parsed_t parse(const std::string &str)
            {
                return parse(llvm::StringRef(str));
            }

            static parsed_t parse(llvm::StringRef str)
            {
               return static_cast< I >(to_int(str));
            }

            static llvm::Type *type(llvm::IRBuilder<> &irb, parsed_t arg)
            {
                return irb.getIntNTy(static_cast< uint32_t >(arg));
            }

            static std::string make(llvm::Type *t)
            {
                auto int_type = llvm::dyn_cast< llvm::IntegerType >(type);
                check(int_type);
                auto size = int_type->getScalarSizeInBits();
                return std::to_string(size);
            }
        };

        struct llvm_type
        {
            using parsed_t = std::string;
            using input_t = llvm::Type *;

            static std::string make(llvm::Type *t)
            {
                auto type_name = [](auto rec, auto type) -> std::string
                {
                    if (auto p_type = llvm::dyn_cast<llvm::PointerType>(type))
                        return "p" + rec(rec, p_type->getPointerElementType());

                    auto int_type = llvm::dyn_cast< llvm::IntegerType >(type);
                    check(int_type);
                    auto size = int_type->getScalarSizeInBits();
                    return std::to_string(size);
                };
                return type_name(type_name, t);
            }

            static parsed_t parse(llvm::StringRef str)
            {
                return str.str();
            }

            static llvm::Type *type(llvm::IRBuilder<> &irb, llvm::Type *t) { return t; }

            template< typename I >
            static I to_bw(const parsed_t &parsed)
            {
                llvm::StringRef str(parsed);
                while (str.consume_front("p")) {}

                return static_cast< I >(to_int(str));
            }
        };

        struct Str
        {
            using parsed_t = std::string;
            using input_t = parsed_t;

            static std::string make(const std::string &str) { return str; }

            static parsed_t parse(llvm::StringRef str) { return str.str(); }
        };

        template< typename E > requires (std::is_enum_v< E >)
        struct IO
        {
            using parsed_t = E;
            using input_t = parsed_t;

            static std::string make(E e)
            {
                using namespace std;
                return to_string(e);
            }

            static E parse(llvm::StringRef str)
            {
                return to_enum< E >::convert(str.str());
            }
        };

        using size = i< std::size_t >;
        using idx = i< std::size_t >;
        using reg = Str;
    }; // namespace suffix

    template< typename T >
    concept suffix_definition = requires (T val)
    {
        { T::parse(std::declval< std::string >()) } -> std::same_as< typename T::parsed_t >;
        { T::make(std::declval< typename T::input_t >()) } -> std::same_as< std::string >;
    };

    template< typename Data, typename ... Es >
    struct SuffixBase
    {
        using self_t = SuffixBase< Data, Es ... >;
        using data_t = Data;
        using parsed_t = std::tuple< typename Es::parsed_t ... >;

        using suffices = tl::TL< Es ... >;

        static inline constexpr std::size_t encoded_args_size = sizeof ... (Es);

        template< typename EHead, typename ... ETail >
        struct Builder
        {
            template< typename AHead, typename ... ATail >
            static std::string make(AHead &&head, ATail && ... tail)
            {
                std::string prefix = EHead::make(std::forward< AHead >(head));

                if constexpr (sizeof ... (ETail) != 0)
                {
                    using rec_t = Builder< ETail ... >;
                    return prefix + Data::delim +
                           rec_t::template make< ATail ... >(std::forward< ATail >(tail) ...);
                } else {
                    return prefix;
                }
            }
        };

        template< typename ... Args >
        static std::string make(Args && ... args)
        {
            using builder_t = self_t::Builder< Es ... >;
            return builder_t::template make< Args ... >(std::forward< Args >(args) ...);
        }

        static parsed_t parse(llvm::StringRef str)
        {
            return _do_parse< Es ... >(str);
        }

        template< typename H, typename ... Tail >
        static auto _do_parse(llvm::StringRef str)
        {
            auto [p, s] = str.split(Data::delim);
            auto current = std::make_tuple(H::parse(p));
            if constexpr (sizeof ... (Tail) != 0)
                return std::tuple_cat(std::move(current), _do_parse< Tail ... >(s));
            else
                return current;
        }
    };

    template< typename, typename > struct Suffix;

    template< typename Data, typename ... Args >
    struct Suffix< Data, tl::TL< Args ... > > : SuffixBase< Data, Args ... > {};

    template< typename ... Args >
    struct _bind_suffix
    {
        template< typename D >
        using bind = Suffix< D, Args ... >;
    };

    template< typename ... Args >
    using bind_suffix = _bind_suffix< Args ... >;

    using size_suffix = bind_suffix< suffix::size >;
    using size_idx_suffix = bind_suffix< suffix::i< uint32_t >, suffix::size >;

    struct config_t
    {
        std::vector< std::string > suffixes;
        std::optional< llvm::Type * > ret_type;
        std::vector< llvm::Type * > args_types;
        bool var_args = false;
    };

    using values_t = std::vector< llvm::Value * >;


    template< typename Definition >
    struct FnBase
    {
        using data_t = Definition;

        static std::tuple< std::string, std::string > parse(llvm::Function *fn)
        {
            auto [ns, raw] = fn->getName().split('.');
            auto [head, tail] = raw.split(Definition::delim);
            return { head.str(), tail.str() };
        }


        static bool is(llvm::Function *fn)
        {
            if (!fn->hasName() || !fn->isDeclaration())
                return false;
            auto name = fn->getName();
            auto eaten = name.consume_front(data_t::fn_prefix);
            return eaten && (name.empty() || name.substr(0, 1) == data_t::delim);
        }

        static bool is(llvm::CallInst *call)
        {
            if (!call->getCalledFunction())
                return false;
            return is(call->getCalledFunction());
        }

        // TODO(lukas): Rework with generator.
        static std::vector<llvm::Function *> all(llvm::Module *module)
        {
            std::vector<llvm::Function *> out;
            for (auto &fn : *module)
                if (is(&fn))
                    out.push_back(&fn);
            return out;
        }

        // TODO(lukas): Investigate if needed with generator.
        template< typename CB >
        static void for_all_in(llvm::Function *trg, CB &&cb)
        {
            auto module = trg->getParent();

            std::vector<llvm::CallInst *> call_insts;
            for (auto fn : all(module))
                for (auto user : fn->users())
                    if (auto call = llvm::dyn_cast<llvm::CallInst>(user);
                        call && call->getParent()->getParent() == trg)
                    {
                        call_insts.push_back(call);
                    }

            for (auto call: call_insts)
              cb(call);
        }

        static void freeze(llvm::Module *mod)
        {
            for (auto fn : all(mod))
                circ::irops::impl::freeze(fn);
        }

        static void melt(llvm::Module *mod)
        {
            for (auto fn : all(mod))
                circ::irops::impl::melt(fn);
        }

    };

    template< typename Definition, typename DStack, typename SStack, typename Modifs >
    struct Fn : Definition, FnBase< Definition >
    {
        using parent_t = FnBase< Definition >;
        using parent_t::parse;

        using suffixes = tl::merge< typename DStack::suffixes, typename SStack::suffixes >;

        static auto build(llvm::IRBuilder<> &irb, const std::vector< config_t > &configs)
            -> llvm::FunctionType *
        {
            std::vector< llvm::Type * > args;
            std::optional< llvm::Type * > ret;
            bool var_args = false;
            for (const auto &config : configs)
            {
                args.insert(args.end(), config.args_types.begin(), config.args_types.end());
                if (config.ret_type)
                    ret = config.ret_type;
                var_args |= config.var_args;
            }

            dcheck(ret, [](){ return "Return type is not specified"; });
            return llvm::FunctionType::get(*ret, args, var_args);
        }

        static std::string make_name(const std::vector< config_t > &configs)
        {
            std::stringstream ss;
            ss << Definition::fn_prefix;
            for (auto &config : configs)
                for (auto &suffix : config.suffixes)
                    ss << Definition::delim << suffix;
            return ss.str();
        }

        template< typename ... Args >
        static auto emit(llvm::IRBuilder<> &irb, const values_t &c_args, Args && ... args)
            -> llvm::CallInst *
        {
            auto fn = create_fn(irb, c_args, std::forward< Args >(args) ...);
            auto call = irb.CreateCall(fn, c_args);
            Modifs::wrap_call(call);

            return call;
        }

        template< typename ... Args >
        static auto create_fn(llvm::IRBuilder<> &irb, const values_t &c_args, Args && ... args)
            -> llvm::Function *
        {
            config_t config;
            DStack::template emit< Definition >(config, irb, c_args);
            SStack::template emit< Definition >(config, irb, std::forward< Args >(args) ...);

            auto llvm_fn_type = build(irb, { config });

            auto name = make_name({ config });

            auto llvm_module = irb.GetInsertBlock()->getModule();
            auto callee = llvm_module->getOrInsertFunction(name, llvm_fn_type);
            auto fn = llvm::dyn_cast< llvm::Function >(callee.getCallee());
            Modifs::wrap_fn(fn);
            return fn;
        }

        static auto parse_args(llvm::Function *fn)
        {
            auto [_, tail] = parse(fn);
            return Suffix< Definition, suffixes >::parse(tail);
        }

    };

    struct State
    {
        llvm::IRBuilder<> &irb;
        config_t &config;

        State(llvm::IRBuilder<> &irb, config_t &config) : irb(irb), config(config) {}
    };

    template< typename ... Layers >
    struct DStack
    {
        using suffixes = tl::merge< typename Layers::suffixes ... >;

        template< typename Definiton >
        static void emit(config_t &out, llvm::IRBuilder<> &irb, const values_t &c_args)
        {
            State state{ irb, out };
            if constexpr (sizeof ... (Layers) != 0)
                run< Layers ... >(state, c_args);
        }

        template< typename L, typename ... Ls >
        static void run(State &state, const values_t &c_args)
        {
            L::emit(state, c_args);
            if constexpr (sizeof ... (Ls) != 0)
                return run< Ls ... >(state, c_args);
            else
                return;
        }
    };

    template< typename R, typename ... Args >
    constexpr auto arg_count(R(*)(Args ...))
    {
        return sizeof ... (Args);
    }

    template< typename ... Layers >
    struct SStack
    {
        using suffixes = tl::merge< typename Layers::suffixes ... >;

        template< typename L, typename ... Ls >
        struct Recurse
        {
            // TODO(lukas): Via some binds I think this can be done in a more generic way,
            //              but we do not require it right now.
            template< typename A, typename ... Args >
            static void run(State &state, A &&a, Args && ... args)
            {
                constexpr auto count = arg_count(L::emit);
                static_assert(count == 1 || count == 2);

                if constexpr (count == 1)
                {
                    L::emit(state);
                    return contd( state, std::forward< A >(a), std::forward< Args >(args) ... );
                } else {
                    L::emit(state, std::forward< A >(a));
                    return contd( state, std::forward< Args >(args) ... );
                }
            }

            template< typename ... Args >
            static auto contd(State &state, Args && ... args)
            {
                if constexpr( sizeof ... (Ls) == 0)
                   return;
                else
                {
                    using next_t = Recurse< Ls ... >;
                    return next_t::run(state, std::forward< Args >(args) ...);
                }
            }

            template< typename ... Args > requires(sizeof ... (Args) == 0)
            static auto run(State &state, Args && ... args)
            {
                static_assert(arg_count(L::emit) - 1 == 0);
                L::emit(state);

                return contd(state);
            }
        };


        template< typename Definiton, typename ... Args >
        static void emit(config_t &out, llvm::IRBuilder<> &irb, Args && ...args)
        {
            State state{ irb, out };

            if constexpr (sizeof ... (Layers) != 0)
            {
                using rec_t = Recurse< Layers ... >;
                rec_t::run(state, std::forward< Args >(args) ... );
            }
        }
    };

    struct NoModifs
    {
        static void wrap_fn(llvm::Function *) {}
        static void wrap_call(llvm::CallInst *) {}
    };

    struct Melts : NoModifs
    {
        static void wrap_fn(llvm::Function *fn) { melt(fn); }
    };

    struct Frozen : NoModifs
    {
        static void wrap_fn(llvm::Function *fn) { freeze(fn); }
    };

    struct UniformSize
    {
        using suffixes = tl::TL< suffix::size >;

        static void emit(State &state, const values_t &c_args)
        {
            auto size = uniform_size(c_args.begin(), c_args.end());
            state.config.suffixes.push_back(suffix::size::make(*size));
            state.config.ret_type = suffix::size::type(state.irb, *size);
        }
    };

    struct SumSize_
    {
        static auto size(const values_t &c_args)
        {
            return sum_sizes(c_args.begin(), c_args.end());
        }
    };

    struct UniformSize_
    {
        static auto size(const values_t &c_args)
        {
            auto size = uniform_size(c_args.begin(), c_args.end());
            check(size,
                  [&](){ return "Operands are not of uniform size\n" + dbg_dump(c_args); });
            return *size;
        }
    };

    template< typename F >
    struct DerivesSize
    {
        using suffixes = tl::TL< suffix::size >;

        static void emit(State &state, const values_t &c_args)
        {
            auto size = F::size(c_args);
            state.config.suffixes.push_back(suffix::size::make(size));
            state.config.ret_type = suffix::size::type(state.irb, size);
        }
    };

    using derives_uniform_size = DerivesSize< UniformSize_ >;
    using derives_sum_size = DerivesSize< SumSize_ >;


    template< std::size_t C >
    struct SizedOps
    {
        using suffixes = tl::TL<>;

        static void emit(State &state)
        {
            dcheck(state.config.ret_type, [](){ return "ret type is not specified!"; });
            for (std::size_t i = 0; i < C; ++i)
                state.config.args_types.push_back(*state.config.ret_type);
        }
    };

    struct NAry
    {
        using suffixes = tl::TL<>;
        static void emit(State &state, const values_t &) { state.config.var_args = true; }
    };

    template< suffix::size::parsed_t size  >
    struct FixSized
    {
        using suf = suffix::size;
        using suffixes = tl::TL< suf >;

        static void emit(State &state)
        {
            state.config.suffixes.push_back(suf::make( size ));
            state.config.ret_type = state.irb.getIntNTy(size);
        }
    };

    template< suffix_definition N >
    struct S
    {
        using suf = N;
        using suffixes = tl::TL< suf >;

        static void emit(State &state, const typename suf::input_t &arg)
        {
            state.config.suffixes.push_back(suf::make((arg)));
        }
    };

    template< suffix_definition N >
    struct R : S< N >
    {
        using suf = typename S< N >::suf;
        using suffixes = typename S< N >::suffixes;

        static void emit(State &state, const typename suf::input_t &arg)
        {
            state.config.ret_type = N::type(state.irb, arg);
            return S< N >::emit(state, arg);
        }
    };

    template< suffix_definition N >
    struct A : S < N >
    {
        using suf = typename S< N >::suf;
        using suffixes = typename S< N >::suffixes;

        static void emit(State &state, const typename suf::input_t &arg)
        {
            state.config.args_types.push_back(N::type(state.irb, arg));
            return S< N >::emit(state, arg);
        }
    };

    /* Helper definitions */

    template< typename Def, typename Derive >
    using nary_derives_size = Fn< Def,
                                  DStack< Derive, NAry >,
                                  SStack<>,
                                  Melts
                                >;

    template< typename, typename D >
    using predicate_base_t = nary_derives_size< D, derives_uniform_size >;

    template< typename, typename D >
    using concat_t = nary_derives_size< D, derives_sum_size >;

    template< typename, typename Def >
    using bin_predicate_t = Fn< Def,
                                DStack< NAry >,
                                SStack< FixSized< 1 > >,
                                Melts
                              >;
    template< typename, typename Def >
    using reg_allocator_t = Fn< Def,
                                DStack<>,
                                SStack< R< suffix::size >,
                                        S< suffix::reg >,
                                        S< suffix::IO< io_type > > >,
                                Melts
                              >;


    template< typename, typename Def >
    using identity_t = Fn< Def,
                           DStack< derives_uniform_size >,
                           SStack< SizedOps< 1 > >,
                           Melts
                         >;

    template< typename, typename Def >
    using select_t = Fn< Def,
                         DStack< NAry >,
                         SStack< R< suffix::size >, A< suffix::size > >,
                         Melts
                       >;

    template< typename, typename Def >
    using idx_allocator_t = Fn< Def,
                                DStack< NAry >,
                                SStack< R< suffix::size >, S< suffix::idx > >,
                                Melts
                              >;
    template< typename X, typename D > using mem_allocator_t = idx_allocator_t< X, D >;

    template< typename, typename Def >
    using raw_allocator_t = Fn< Def,
                                DStack< NAry >,
                                SStack< R< suffix::llvm_type > >,
                                Frozen
                              >;

    template< typename Def, std::size_t size >
    using fixed_leaf_t = Fn< Def,
                             DStack<>,
                             SStack< FixSized< size >, S< suffix::IO< io_type > > >,
                             Melts
                           >;

    template< typename, typename Def >
    using extract_t = Fn< Def,
                          DStack< NAry >,
                          SStack< S< suffix::size >, R< suffix::size > >,
                          Melts
                        >;

    template< typename, typename Def >
    using operand_leaf_t = Fn< Def,
                               DStack< NAry >,
                               SStack< R< suffix::size >, S< suffix::idx > >,
                               Melts
                             >;

    template< typename, typename Def >
    using unary_check_t = Fn< Def,
                              DStack<>,
                              SStack< FixSized< 1 >, SizedOps< 1 > >,
                              Melts
                            >;
    template< typename, typename Def >
    using frozen_predicate_t = Fn< Def,
                                   DStack< NAry >,
                                   SStack< FixSized< 1 >>,
                                   Frozen
                                 >;

    template< typename, typename Def >
    using type_idx_t = Fn< Def,
                           DStack< NAry >,
                           SStack< R< suffix::llvm_type >, S< suffix::idx > >,
                           Melts
                         >;

} // namespace circ::irops::impl
