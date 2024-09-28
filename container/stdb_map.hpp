#include <string>
#include <type_traits>
#include "container/stdb_vector.hpp"
#include "arena/arena.hpp"

namespace stdb::container {
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

template <typename Mapped>
constexpr bool is_map_v = !std::is_void_v<Mapped>;

namespace bucket_type {

struct standard {
    static constexpr uint32_t dist_inc = 1U << 8U;             // skip 1 byte fingerprint
    static constexpr uint32_t fingerprint_mask = dist_inc - 1; // mask for 1 byte of fingerprint

    uint32_t m_dist_and_fingerprint; // upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    uint32_t m_value_idx;            // index into the m_values vector.
};

} // namespace bucket_type

// base type for map has mapped_type
template <class K>
struct base_table_type_map {
    using mapped_type = K;
};

// base type for set doesn't have mapped_type
struct base_table_type_set {};


// Partial specialization for types satisfying IsString (i.e., when IsString<T> is true)
template <IsStringOrView Key,
          typename Value,
          typename Hash = std::hash<Key>,
          typename SecondHash = void,
          typename KeyEqual = std::equal_to<Key>,
          typename Bucket = bucket_type::standard,
          typename BucketAllocator = typename std::allocator<Bucket>,
          size_t MaxKeySize = 0>  // 0 means no key will be copied into Arena
class StringMap : public std::conditional_t<is_map_v<Value>, base_table_type_map<Value>, base_table_type_set> {
    // if Value is void, then Value is Key
    // and if SecondHash is not Void, the Key should be std::pair<Hash, Key>
    using underlying_value_type = typename std::conditional_t<
      is_map_v<Value>, typename std::conditional_t<std::is_void_v<SecondHash>, Key, std::pair<Key, Value>>, Key>;

   private:
    stdb::memory::Arena& _arena;
    static constexpr uint8_t kInitialShift = 64 - 2; // 2^(64-m_shift) number of buckets
    static constexpr float kDefaultMaxLoadFactor = 0.8F;
    using bucket_allocator = BucketAllocator;
    using bucket_type = Bucket;
    using bucket_ptr = Bucket*;
    using hasher = Hash;
    using second_hasher = SecondHash;

   private:
    bucket_ptr _buckets;
    size_t _num_buckets;
    size_t _max_bucket_capacity;
    float _max_load_factor = kDefaultMaxLoadFactor;
    Hash _hash{};
    SecondHash _second_hash{};
    KeyEqual _equal{};
    uint8_t _shifts = kInitialShift;
   public:
    // the value will be stored in the arena
    // the Arena's layout is:
    // | keylength | key hash | value |... key ... |
    // or if SecondHash is void:
    // | keylength | value | ... key ... |

    // should always make sure the value is fixed size.(no string in value)


    // delete default constructor, because we need to initialize _arena
    StringMap() = delete;

    // delete copy constructor and assignment operator, because we don't want to copy the arena
    StringMap(const StringMap&) = delete;
    auto operator=(const StringMap&) -> StringMap& = delete;

    // move constructor and assignment operator is ok
    StringMap(StringMap&&) noexcept = default;

    // just move cstr is ok.
    auto operator=(StringMap&&) noexcept -> StringMap& = delete;

    // the <key, values> pair will be stored in the arena, and do not destruct by self.
    ~StringMap() {
        free(_buckets);
    }

    explicit StringMap(stdb::memory::Arena& arena) : _arena(arena) {}

};  // class StringMap of String
    



} // namespace stdb::container