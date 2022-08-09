/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include "IR.hpp"

namespace circ {

template<typename T, typename ...Ts>
bool IsOneOf(Operation *op) {
  if (op->op_code == T::kind) {
    return true;
  }

  if constexpr (sizeof...(Ts) == 0) {
    return false;
  } else {
    return IsOneOf<Ts...>(op);
  }
}

static inline bool IsLeafNode(Operation *op) {
  switch(op->op_code) {
    case InputRegister::kind:
    case OutputRegister::kind:
    case Constant::kind:
    case Advice::kind:
    case Undefined::kind:
    case InputInstructionBits::kind:
      return true;
    default:
      return false;
  }
}

template<typename T>
bool Is(Operation *op) {
  return op->op_code == T::kind;
}

using operation_set_t = std::unordered_set<Operation *>;

template<typename T>
struct SubtreeCollector {
  std::unordered_multiset<T *> collected;

  template<typename C>
  SubtreeCollector<T> &Run(C &&ops) {
    for (auto op : ops) {
      Run(op);
    }
    return *this;
  }

  SubtreeCollector<T> &Run(Operation *o) {
    if (o->op_code == T::kind) {
      collected.insert(dynamic_cast<T *>(o));
    }
    for (auto op : o->operands()) {
      Run(op);
    }
    return *this;
  }

  template<typename CB>
  auto Apply(CB cb) {
    using res_t = decltype(cb(*collected.begin()));
    std::vector<res_t> out;
    for (auto x : collected) {
      out.push_back(cb(x));
    }
    return out;
  }
};



namespace print {
  template<typename Derived>
  struct Topology {
    std::stringstream ss;
    using hash_t = std::string;

    Derived &Self() { return static_cast<Derived &>(*this); }

    template<typename C>
    std::string Hash(const C& ops) {
      std::stringstream hash;
      for (auto op : ops) {
        hash << Hash(op);
        hash << " | ";
      }
      return hash.str();
    }

    std::string Hash(Operation *op) {
      return Self().Print(op, 0);
    }

    std::string Children(Operation *op, uint8_t depth) {
      std::string out;
      for (auto o : op->operands()) {
        out += Self().Print(o, depth + 1);
        out += Self().separator;
      }
      return out;
    }

    std::string Print(Operation *op) {
      return Self().Print(op, 0);
    }

    std::string Print(Operation *op, uint8_t depth) {
      auto indent = Self().Indent(depth);
      std::string out;
      out += indent;
      out += Self().Op(op);
      out += "( ";
      out += Self().Children(op, depth);
      out += indent + ")";
      return out;
    }

    std::string Get() { return ss.str(); }
    std::string Indent(uint8_t) { return {}; }
  };

  template<typename Next>
  struct WithCache : Next {
    using parent_t = Next;
    using hash_t = typename parent_t::hash_t;

    using Next::Hash;

    std::unordered_map<Operation *, hash_t > op_to_hash;

    std::string Print(Operation *op, uint8_t depth) {
      auto it = op_to_hash.find(op);
      if (it != op_to_hash.end()) {
        return it->second;
      }
      auto x = this->parent_t::Print(op, depth + 1);
      op_to_hash[op] = x;
      return x;
    }
  };

  template<typename Next>
  struct FullNames_ : Next {
    static inline constexpr const char separator = ' ';
    std::string Op(Operation *op) { return op->name(); }
  };

  struct FullNames : FullNames_<WithCache<Topology<FullNames>>> {};

  struct PrettyPrinter : FullNames {
    std::string Indent(uint8_t depth) {
      return std::string(depth * 2, ' ');
    }
  };

} // namespace print



namespace collect {
  struct Ctxs {
    using ctxs_t = std::unordered_set<Operation *>;
    using ctxs_map_t = std::unordered_map<Operation *, ctxs_t>;

    ctxs_map_t op_to_ctxs;

    void Root(Operation *op) {
      op_to_ctxs[op] = { op };
    }

    void Update(Operation *node, Operation *user) {
      if (user) {
        auto &ctxs = op_to_ctxs[node];
        auto &user_ctxs = op_to_ctxs[user];
        ctxs.insert(user_ctxs.begin(), user_ctxs.end());
      }
    }
  };

  struct Hashes : print::FullNames {
    void Root(Operation *op) {
      Hash(op);
    }

    void Update(Operation *node, Operation *user) {
      check(op_to_hash.count(node));
    }
  };

  struct AllowsUndef {
    std::optional< bool > allows;

    void Root(Operation *op) {}

    void Update(Operation *node, Operation *user) {
      if (node->op_code == Undefined::kind)
        allows = true;
    }
  };


  template< typename ...Ts >
  struct UpTree {
    std::unordered_set<Operation *> collected;

    void Run(Operation *op) {
      if (is_one_of<Ts...>(op)) {
        collected.insert(op);
      }
      for (auto o : op->users()) {
        Run(o);
      }
    }
  };


  template< typename ...Ts >
  struct DownTree {
      std::unordered_set<Operation *> collected;

      void Run(Operation *op) {
          if (is_one_of<Ts...>(op)) {
              collected.insert(op);
          }
          for (auto o : op->operands()) {
              Run(o);
          }
      }
  };
} // namespace collect

template<typename ...Collectors>
struct Collector : Collectors ... {
  using self_t = Collector<Collectors...>;

  using entry_t = std::pair<Operation *, Operation *>;
  std::deque<entry_t> todo;

  self_t &Run(Circuit *circuit) {
    for (auto x : circuit->attr<VerifyInstruction>()) {
      (Collectors::Root(x), ...);
      todo.push_back({x, nullptr});
    }

    while (!todo.empty()) {
      const auto &[x, y] = todo.front();
      todo.pop_front();
      Update(x, y);
    }
    return *this;
  }

  void Update(Operation *node, Operation *user) {
    (Collectors::Update(node, user), ...);
    for (auto op : node->operands()) {
      todo.emplace_back(op, node);
    }
  }
};

using CtxCollector = Collector<collect::Ctxs>;

static inline bool allows_undef_(Operation *op, std::unordered_set< Operation * > &seen)
{
  if (seen.count(op)) return false;
  seen.insert(op);

  if (op->op_code == Undefined::kind)
    return true;

  for (auto x : op->operands())
    if (allows_undef_(x, seen))
      return true;
  return false;
}
static inline bool allows_undef(Operation *op) {
  if (op->op_code != RegConstraint::kind ||
      op->operand(1)->op_code != OutputRegister::kind)
  {
    return false;
  }
  std::unordered_set< Operation * > seen;
  return allows_undef_(op, seen);
}

static inline Operation *GetContext(Operation *op) {
  collect::UpTree<VerifyInstruction> collector;
  collector.Run(op);
  auto &ctxs = collector.collected;
  check(ctxs.size() == 1);
  return *(ctxs.begin());
}

static inline std::unordered_set<Operation *> GetContexts(Operation *op) {
  collect::UpTree<VerifyInstruction> up_collector;
  up_collector.Run( op);

  SubtreeCollector< VerifyInstruction > down_collector;
  auto down_collected = std::move(down_collector.Run( op ).collected);

  up_collector.collected.insert(down_collected.begin(), down_collected.end());
  return std::move(up_collector.collected);
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

    // By keeping this logic located inside dispatch we allow the user which direction to traverse
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
                path_to_save.emplace(path_to_save.begin(), op ); // op hasn't been added to the path just yet
                collected.push_back( path_to_save ); // we want this explicit copy
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
    down_collector.Run( op );
    for (auto &o: down_collector.collected) {
        vis.visit( o );
    }
}
} // namespace circ
