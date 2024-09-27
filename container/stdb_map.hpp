#include <string>
#include <type_traits>
#include "container/stdb_vector.hpp"

namespace stdb::container {

#include <type_traits>
#include <cstddef>
#include <string>
#include <string_view>

// Concepts definitions
template<typename T>
concept IsString = requires(T t) {
    { t.data() } -> std::same_as<const char*>;
    { t.size() } -> std::same_as<size_t>;
    { t.shrink_to_fit() } -> std::same_as<void>;
};

template <typename T>
concept IsStringOrView = requires(T t) {
    { t.data() } -> std::same_as<const char*>;
    { t.size() } -> std::same_as<size_t>;
};

template <typename T>
concept IsStringView = IsStringOrView<T> && not IsString<T>;

// Primary template
template <typename T, typename Enable = void>
class StringMap;

// Partial specialization for types satisfying IsString (i.e., when IsString<T> is true)
template <typename T>
class StringMap<T, std::enable_if_t<IsString<T>>> {
public:
    
};  // class StringMap of String

// Partial specialization for types satisfying IsStringView (i.e., when IsStringView<T> is true)
template <typename T>
class StringMap<T, std::enable_if_t<IsStringView<T>>> {
public:
    
};  // class StringMap of StringView



} // namespace stdb::container