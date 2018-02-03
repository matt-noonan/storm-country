
#include <variant>
#include <functional>
#include <vector>
#include <memory>
#include <utility>
#include <iostream>
#include <cassert>
#include <string>

// C++17 for std::variant, or modify for use with boost::variant

// Define the variants for the IntF functor:
template <typename T> struct Zero {};
template <typename T> struct One  {};
template <typename T> struct Add  { T x; T y; };
template <typename T> struct Mul  { T x; T y; };
template <typename T> struct Neg  { T x; };

// Now use std::variant to create the sum type.
template <typename T>
using IntF = std::variant< Zero <T>
                         , One  <T>
                         , Add  <T>
                         , Mul  <T>
                         , Neg  <T> >;

template <typename T>
using IntF_algebra = std::function<T(IntF<T> const&)>;


// Implement an F-algebra analogous to IntF Integer -> Integer
int alg (IntF<int> const& v)
{
  if ( std::holds_alternative<Zero<int>>(v) ) {
      return 0;
  }
  else if ( std::holds_alternative<One<int>>(v) ) {
      return 1;
  }
  else if ( std::holds_alternative<Add<int>>(v) ) {
      auto add = std::get<Add<int>>(v);
      return add.x + add.y;
  }
  else if ( std::holds_alternative<Mul<int>>(v) ) {
      auto mul = std::get<Mul<int>>(v);
      return mul.x * mul.y;
  }
  else {
    assert ( std::holds_alternative<Neg<int>>(v) );
    auto neg = std::get<Neg<int>>(v);
    return -neg.x;
  }
}

// Implement an F-algebra analogous to IntF Integer -> Integer
bool algB (IntF<bool> const& v)
{
  if ( std::holds_alternative<Zero<bool>>(v) ) {
      return false;
  }
  else if ( std::holds_alternative<One<bool>>(v) ) {
      return true;
  }
  else if ( std::holds_alternative<Add<bool>>(v) ) {
      auto add = std::get<Add<bool>>(v);
      return add.x || add.y;
  }
  else if ( std::holds_alternative<Mul<bool>>(v) ) {
      auto mul = std::get<Mul<bool>>(v);
      return mul.x && mul.y;
  }
  else {
    assert ( std::holds_alternative<Neg<bool>>(v) );
    auto neg = std::get<Neg<bool>>(v);
    return !neg.x;
  }
}

template <typename T>
struct property
{
    virtual ~property() {}
    
    virtual std::string describe() const = 0;
    virtual T lhs() const = 0;
    virtual T rhs() const = 0;
    
    bool check() const
    { return lhs() == rhs(); }
};

template <typename T>
struct prop_commutative_add : property<T>
{
    IntF_algebra<T> alg;
    T x; T y;

    prop_commutative_add (IntF_algebra<T> alg_, T x_, T y_)
    : alg(alg_), x(x_), y(y_) {}

    std::string describe() const
    { return "addition is commutative"; }
    
    T lhs() const
    { return alg(Add<T> { x, y }); }
    
    T rhs() const
    { return alg(Add<T> { y, x }); }
};

template <typename T>
struct zero_neutral_L : property<T>
{
    IntF_algebra<T> alg;
    T x;

    zero_neutral_L (IntF_algebra<T> alg_, T x_)
    : alg(alg_), x(x_) {}
    
    std::string describe() const
    { return "adding zero on the left has no effect"; }
    
    T lhs() const
    {
        auto zero = alg(Zero<T>{});
        return alg(Add<T> { zero, x });
    }

    T rhs() const
    { return x; }
};

template <typename T>
struct zero_neutral_R : property<T>
{
    IntF_algebra<T> alg;
    T x;
    
    zero_neutral_R (IntF_algebra<T> alg_, T x_)
    : alg(alg_), x(x_) {}
    
    std::string describe() const
    { return "adding zero on the right has no effect"; }
    
    T lhs() const
    {
        auto zero = alg(Zero<T>{});
        return alg(Add<T> { x, zero });
    }

    T rhs() const
    { return x; }
};

int main(void)
{
	std::cout << alg(Zero<int> {})       << std::endl;
	std::cout << alg(One <int> {})       << std::endl;
	std::cout << alg(Add <int> { 3, 4 }) << std::endl;
	std::cout << alg(Mul <int> { 3, 4 }) << std::endl;
	std::cout << alg(Neg <int> { 42 })   << std::endl;

    std::cout << std::endl;
	std::cout << algB(Zero<bool> {})       << std::endl;
	std::cout << algB(One <bool> {})       << std::endl;
	std::cout << algB(Add <bool> { true, false }) << std::endl;
	std::cout << algB(Mul <bool> { true, false }) << std::endl;
	std::cout << algB(Neg <bool> { true })   << std::endl;

    std::vector<std::shared_ptr<property<int>>> props
    { std::make_shared<zero_neutral_R<int>>( alg, 42 )
    , std::make_shared<zero_neutral_L<int>>( alg, 42 )
    , std::make_shared<prop_commutative_add<int>>( alg, 3, 4 )
    };

    for (auto prop : props) {
        std::cout << "testing if "
                  << prop->describe() << std::endl;
        if (prop->check()) {
            std::cout << " [passed]" << std::endl;
        }
        else {
            std::cout << " [FAIL]" << std::endl;
            std::cout << "    " << prop->lhs()
                      << " != " << prop->rhs() << std::endl;
        }
    }
	return 0;
}