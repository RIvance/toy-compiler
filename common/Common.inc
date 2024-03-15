
#ifndef COMMON_INC
#define COMMON_INC

#include <cstddef>
#include <cmath>
#include <cstdint>
#include <cassert>
#include <memory>
#include <functional>
#include <utility>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <stack>
#include <deque>
#include <list>
#include <mutex>

#include "Alias.inc"


using usize = std::size_t;

using i8  = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;
using u8  = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;

using f32 = std::float_t;
using f64 = std::double_t;

using Int8   = i8;
using Int16  = i16;
using Int32  = i32;
using Int64  = i64;
using UInt8  = u8;
using UInt16 = u16;
using UInt32 = u32;
using UInt64 = u64;

using Float32 = f32;
using Float64 = f64;

using Bool = bool;
using Char = char;

using String = std::string;
FUNCTION_ALIAS(str, std::to_string);

using Mutex = std::mutex;

GENERIC_TYPE_ALIAS(Ref = std::shared_ptr<T>, typename T);
GENERIC_TYPE_ALIAS(Box = std::unique_ptr<T>, typename T);
GENERIC_TYPE_ALIAS(WeakRef = std::weak_ptr<T>, typename T);
GENERIC_TYPE_ALIAS(LockGuard = std::lock_guard<T>, typename T);
GENERIC_FUNCTION_ALIAS(some, std::make_optional<T>, typename T);
GENERIC_FUNCTION_ALIAS(ref, std::make_shared<T>, typename T);
GENERIC_FUNCTION_ALIAS(box, std::make_unique<T>, typename T);
// GENERIC_TYPE_ALIAS(Function = std::function<Signature>, typename Signature);

#define $ ,

GENERIC_TYPE_ALIAS(Option = std::optional<T>, typename T);
GENERIC_TYPE_ALIAS(Vector = std::vector<T>, typename T);
GENERIC_TYPE_ALIAS(TreeMap = std::map<K $ V>, typename K, typename V);
GENERIC_TYPE_ALIAS(HashMap = std::unordered_map<K $ V>, typename K, typename V);
GENERIC_TYPE_ALIAS(TreeSet = std::set<T>, typename T);
GENERIC_TYPE_ALIAS(HashSet = std::unordered_set<T>, typename T);
GENERIC_TYPE_ALIAS(Stack = std::stack<T>, typename T);
GENERIC_TYPE_ALIAS(LinkedList = std::list<T>, typename T);

#undef $

template <typename D, typename B>
concept Derives = std::is_base_of_v<B, D>;

template <typename T>
concept Cloneable = std::is_copy_constructible_v<T>;

template <typename T>
concept Destructible = std::is_trivially_destructible_v<T>;

template <typename TCollection>
concept Iterable = requires(TCollection iterable) {
    { iterable.begin() } -> std::template same_as<decltype(std::begin(iterable))>;
    { iterable.end() } -> std::template same_as<decltype(std::end(iterable))>;
};

template <typename T>
concept PointerCloneable = requires(T obj) {
    { obj.clone() } -> std::same_as<T*>;
};

template <typename T>
concept Default = std::is_default_constructible_v<T>;

template <typename F, typename T>
concept Consumer = requires(F consumer, T value) {
    { consumer(value) } -> std::same_as<void>;
};

template <typename F, typename Key, typename Value>
concept KeyValueConsumer = requires(F consumer, Key key, Value value) {
    { consumer(key, value) } -> std::same_as<void>;
};

template <typename F, typename R>
concept Producer = requires(F producer) {
    { producer() } -> std::same_as<R>;
};

template <typename F, typename T>
concept Predicate = requires(F predicate, const T& value) {
    { predicate(value) } -> std::same_as<bool>;
};

template <typename F, typename K, typename V>
concept KeyValuePredicate = requires(F predicate, K key, V value) {
    { predicate(key, value) } -> std::same_as<bool>;
};

template <typename F, typename T, typename R>
concept Mapper = requires(F mapper, T&& t) {
    { mapper(t) } -> std::same_as<R>;
};

template <typename  F, typename T, typename R>
concept Reducer = requires(F reducer, T value, R acc) {
    { reducer(acc, value) } -> std::same_as<R>;
};

template <typename F, typename T1, typename T2>
concept Matcher = requires(F matcher, T1 t1, T2 t2) {
    { matcher(t1, t2) } -> std::same_as<bool>;
};

template <typename T>
concept Comparable = requires(T a, T b) {
    { a == b } -> std::same_as<bool>;
};

template <typename F, typename T>
concept Comparator = requires(F cmp, const T& a, const T& b) {
    { cmp(a, b) } -> std::same_as<bool>;
};

struct NotImplementedException final : std::exception
{
    const char* message = "Not implemented";
    NotImplementedException() noexcept = default;
    explicit NotImplementedException(const char* message) : message(message) { }
    [[nodiscard]] const char* what() const noexcept override { return message; }
};

struct UnreachableException final : std::exception
{
    const char* message = "Unreachable code";
    UnreachableException() noexcept = default;
    explicit UnreachableException(const char* message) : message(message) { }
    [[nodiscard]] const char* what() const noexcept override { return message; }
};

#define TODO(...) throw NotImplementedException(__VA_ARGS__)
#define unreachable(...) throw UnreachableException(__VA_ARGS__)

#define ABSTRACT_FUNCTION = 0
#define castPtr reinterpret_cast

#define NAMESPACE_BEGIN(ns)         namespace ns {
#define NAMESPACE_END               }

#define interface struct
#define def auto
#define let const auto

#endif // COMMON_INC
