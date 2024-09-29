#include <memory>
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

template<size_t BlockSize>
struct small {
    static constexpr uint32_t dist_inc = 1U << 8U;             // skip 1 byte fingerprint
    static constexpr uint32_t fingerprint_mask = dist_inc - 1; // mask for 1 byte of fingerprint

    uint32_t dist_and_fingerprint; // upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    uint32_t key_offset; // offset of the key in the arena, the assumption is the arena's blocks have same size.

    [[nodiscard]] auto equal(uint32_t other_dist_and_fingerprint, std::string_view other_key) const -> bool {
        return other_dist_and_fingerprint == this->dist_and_fingerprint && other_key == extract_key();
    }

    [[nodiscard]] auto extract_key(memory::Arena& arena) const -> std::string_view {
        // need change Arena's API to get the key
        return {};
    }

    template<typename T>
    [[nodiscard]] auto extract_value(memory::Arena& arena) const -> T {
        // need change Arena's API to get the value
        return {};
    }

}; // struct small

static_assert(sizeof(small<4>) == sizeof(void*), "small bucket size should be 16");

struct mediam {
    static constexpr uint32_t dist_inc = 1U << 8U;             // skip 1 byte fingerprint
    static constexpr uint64_t fingerprint_mask = dist_inc - 1; // mask for 1 byte of fingerprint

    uint64_t dist_and_fingerprint; // upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    uint16_t key_length; // key length, if key length is greater than 2**16, then use 0
    uint64_t key_address:48; 

    [[nodiscard]] auto equal(uint32_t other_dist_and_fingerprint, std::string_view other_key) const -> bool {
        return other_dist_and_fingerprint == this->dist_and_fingerprint && other_key == extract_key();
    }

    [[nodiscard]] auto extract_key() const -> std::string_view {
        // NOLINTNEXTLINE(performance-no-int-to-ptr)
        return {reinterpret_cast<const char*>(key_address), key_length};
    }

    template<typename T>
    [[nodiscard]] auto extract_value() -> T {
        // NOLINTNEXTLINE(performance-no-int-to-ptr)
        return {*reinterpret_cast<const T*>(key_address + key_length)};
    }
}; // struct mediam

static_assert(sizeof(mediam) == 2 * sizeof(void*), "mediam bucket size should be 16");

struct double_hash_mediam {
    static constexpr uint32_t dist_inc = 1U << 8U;             // skip 1 byte fingerprint
    static constexpr uint32_t fingerprint_mask = dist_inc - 1; // mask for 1 byte of fingerprint

    uint32_t dist_and_fingerprint; // upper 3 byte: distance to original bucket. lower byte: fingerprint from hash
    uint32_t second_hash; // will need more computation for once more hashing while emplace or find
    uint16_t key_length; // key length, if key length is greater than 2**16, then use 0
    uint64_t key_address:48; 

    [[nodiscard]] auto equal(uint32_t other_dist_and_fingerprint, uint32_t other_second_hash, std::string_view other_key) const -> bool {
        return other_dist_and_fingerprint == this->dist_and_fingerprint && other_second_hash == this->second_hash && other_key == extract_key();
    }

    [[nodiscard]] auto extract_key() const -> std::string_view {
        // NOLINTNEXTLINE(performance-no-int-to-ptr)
        return {reinterpret_cast<const char*>(key_address), key_length};
    }

    template<typename T>
    [[nodiscard]] auto extract_value() const -> T {
        // NOLINTNEXTLINE(performance-no-int-to-ptr)
        return {*reinterpret_cast<const T*>(key_address + key_length)};
    }


}; // struct double_hash_mediam

static_assert(sizeof(double_hash_mediam) == 2 * sizeof(void*), "double_hash_mediam bucket size should be 16");
} // namespace bucket_type

// // base type for map has mapped_type
// template <class K>
// struct base_table_type_map {
//     using mapped_type = K;
// };

// // base type for set doesn't have mapped_type
// struct base_table_type_set {};


// Partial specialization for types satisfying IsString (i.e., when IsString<T> is true)
template <IsStringOrView Key,
          typename Value,
          typename Hash = std::hash<Key>,
          typename SecondHash = void,
          typename Bucket = bucket_type::mediam,
          typename BucketAllocator = typename std::allocator<Bucket>,
          size_t MaxKeySize = 0>  // 0 means no key will be copied into Arena
// class StringMap : public std::conditional_t<is_map_v<Value>, base_table_type_map<Value>, base_table_type_set> {
class StringMap {
    // if Value is void, then Value is Key
    // and if SecondHash is not Void, the Key should be std::pair<Hash, Key>

   public:
    class Iterator {
        using bucket_ptr = const Bucket*;
       private:
        bucket_ptr _ptr = nullptr;

       public:
        explicit Iterator(bucket_ptr ptr): _ptr(ptr) {}

        auto operator*() const -> std::pair<Key, Value> {
            return {_ptr->extract_key(), _ptr->extract_value()};
        }

        auto operator->() const -> std::pair<Key, Value>* {
            return _ptr;
        }

        auto operator++() -> Iterator& {
            _ptr++;
            return *this;
        }

        auto operator++(int) -> Iterator {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        auto operator==(const Iterator& other) const -> bool {
            return _ptr == other._ptr;
        }

        auto operator!=(const Iterator& other) const -> bool {
            return !(*this == other);
        }
    }; // class Iterator

   private:
    stdb::memory::Arena& _arena;
    static constexpr uint8_t kInitialShift = 64 - 2; // 2^(64-m_shift) number of buckets
    static constexpr float kDefaultMaxLoadFactor = 0.8F;
    using bucket_allocator = BucketAllocator;
    using bucket_type = Bucket;
    using bucket_ptr = Bucket*;
    using bucket_index_type = uint32_t;
    using hasher = Hash;
    using second_hasher = SecondHash;
    using block_difference_type = std::pointer_traits<bucket_type*>::difference_type;
    using const_iterator = Iterator;

   private:
    using dist_and_fingerprint_type = decltype(Bucket::dist_and_fingerprint);
    bucket_ptr _buckets{};
    size_t _nbuckets = 0;
    size_t _max_bucket_capacity = 0;
    float _max_load_factor = kDefaultMaxLoadFactor;
    Hash _hash{};
    SecondHash _second_hash{};
    uint8_t _shifts = kInitialShift;

    [[nodiscard]] constexpr auto dist_and_fingerprint_from_hash(uint64_t hash) const -> dist_and_fingerprint_type {
        return Bucket::dist_inc | (static_cast<dist_and_fingerprint_type>(hash) & Bucket::fingerprint_mask);
    }

    [[nodiscard]] constexpr auto bucket_idx_from_hash(uint64_t hash) const -> bucket_index_type {
        return static_cast<bucket_index_type>(hash >> _shifts);
    }
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

    [[nodiscard]] auto empty() const -> bool {
        // TODO: implement this
        return false;
    }

    [[nodiscard]] auto end() const -> const_iterator {
        return {};
    }
    
    [[nodiscard]] auto end() -> iterator {
        return {};
    }

   private:
    [[nodiscard]] static constexpr auto at(bucket_ptr bucket, size_t offset) -> Bucket& {
        return *(bucket + static_cast<block_difference_type>(offset));
    }

    [[nodiscard]] static constexpr auto dist_increase(dist_and_fingerprint_type x) -> dist_and_fingerprint_type {
        return static_cast<dist_and_fingerprint_type>(x + Bucket::dist_inc);
    }

    [[nodiscard]] static constexpr auto dist_decrease(dist_and_fingerprint_type x) -> dist_and_fingerprint_type {
        return static_cast<dist_and_fingerprint_type>(x - Bucket::dist_inc);
    }

    [[nodiscard]] auto next(bucket_index_type bucket_idx) -> bucket_index_type {
        return bucket_idx + 1UL == _nbuckets ? 0 : bucket_idx + 1UL;
    }

    [[nodiscard, gnu::always_inline]] inline auto bucket_count() const noexcept -> size_t {
        return _nbuckets;
    }

    void clear_buckets() {
        if (_buckets != nullptr) {
            std::memset(_buckets, 0, sizeof(Bucket) * bucket_count());
        }
    }

    template<typename K>
    [[nodiscard]] constexpr auto mixed_hash(K const& key) const -> uint64_t {
        return _hash(key);
    }

    template<typename K>
    auto do_find(K const& key) -> const_iterator {
        if (empty()) [[unlikely]] {
            return end();
        }
        // generate the mixed hash
        auto the_mixed_hash = mixed_hash(key);
        auto dist_and_fingerprint = dist_and_fingerprint_from_hash(the_mixed_hash);
        auto bucket_idx = bucket_idx_from_hash(the_mixed_hash);
        auto* bucket = &at(_buckets, bucket_idx);

        // unrolled loop. *Always* check a few directly, then enter the loop. This is faster.
        if (bucket->equal(dist_and_fingerprint, key)) {
            return iterator(bucket);
        }

        // the second loop
        dist_and_fingerprint = dist_increase(dist_and_fingerprint);
        bucket_idx = next(bucket_idx);
        bucket = &at(_buckets, bucket_idx);

        if (bucket->equal(dist_and_fingerprint, key)) {
            return iterator(bucket);
        }

        // into the loop
        dist_and_fingerprint = dist_increase(dist_and_fingerprint);
        bucket_idx = next(bucket_idx);
        bucket = &at(_buckets, bucket_idx);

        while (true) {
            if (bucket->equal(dist_and_fingerprint, key)) {
                return iterator(bucket);
            }
            if (dist_and_fingerprint > bucket->dist_and_fingerprint) {
                return end();
            }

            dist_and_fingerprint = dist_increase(dist_and_fingerprint);
            bucket_idx = next(bucket_idx);
            bucket = &at(_buckets, bucket_idx);
        }
    }

    template<typename K>
    auto do_find(K const& key) const -> const_iterator {
        return do_find(key);
    }


    template<typename ... Args>
    auto do_place_element(dist_and_fingerprint_type dist_and_fingerprint, bucket_index_type bucket_idx  , Args&&... args) -> std::pair<const_iterator, bool> {

    }

    template<typename K, typename... Args>
    auto do_try_emplace(K&& key, Args&&... args) -> std::pair<const_iterator, bool> {
    }
};  // class StringMap of String
    



} // namespace stdb::container