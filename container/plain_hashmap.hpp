#include <cstdint>
#include <utility>
#include <limits>
#include <algorithm>
#include <cstring>
#include "string/string.hpp"
#include <concepts>
#include <string_view>
#include <type_traits>

namespace stdb::container {

// StringLike is a concept that checks if a type is a string-like type.
// It checks if the type has data() and size() methods that return the correct type.
template <typename S>
concept StringLike = requires(S str) {
    // s.data() should return a char pointer or const char pointer
    {str.data()} -> std::same_as<const char*>;
    // s.size() should return a size_t
    {str.size()} -> std::same_as<size_t>;
} || requires(S str) {
    // s.data() should return a char pointer or const char pointer
    {str.data()} -> std::same_as<char*>;
    // s.size() should return a size_t
    {str.size()} -> std::same_as<size_t>;
};

namespace bucket_type {

constexpr uint32_t dist_inc = 1U << 8U;

static_assert(StringLike<memory::string>);
static_assert(StringLike<std::string>);
static_assert(StringLike<std::string_view>);

template <std::integral Key, typename Value>
    requires(sizeof(Key) <= sizeof(uint32_t))
struct compact_32b_map_bucket
{
    uint32_t distance = {};
    union {
        Key key = {};
        uint32_t hash;
    };
    Value value = {};
};  // struct compact_int_key

static_assert(sizeof(compact_32b_map_bucket<uint32_t, uint32_t>) == 3 * sizeof(uint32_t), "compact_int_key should be 12 bytes");
static_assert(sizeof(compact_32b_map_bucket<int32_t, uint64_t>) == 2 * sizeof(uint64_t), "compact_int_key should be 16 bytes");


template<std::integral Key, typename Value = void>
    requires(sizeof(Key) <= sizeof(uint32_t))
struct compact_32b_set_bucket {
    uint32_t distance = {};
    union {
        Key key = {};
        uint32_t hash;
    };
};  // struct compact_32b_set_bucket

static_assert(sizeof(compact_32b_set_bucket<uint32_t>) == 2 * sizeof(uint32_t), "compact_32b_set_bucket should be 8 bytes");
static_assert(sizeof(compact_32b_set_bucket<uint16_t>) == 2 * sizeof(uint32_t), "compact_32b_set_bucket should be 8 bytes");

template <std::integral Key, typename Value>
    requires(sizeof(Key) == sizeof(uint64_t))
struct compact_64b_key_bucket {
    uint32_t distance = {};
    union {
        Key key = {};
        uint64_t hash;
    };
    Value value = {};
};  // struct compact_long_key_bucket

static_assert(sizeof(compact_64b_key_bucket<uint64_t, uint64_t>) == 3 * sizeof(uint64_t), "compact_long_key_bucket should be 24 bytes");
static_assert(sizeof(compact_64b_key_bucket<int64_t, memory::string>) == 5 * sizeof(uint64_t), "compact_long_key_bucket should be 40 bytes");

template<std::integral Key, typename Value = void>
    requires(sizeof(Key) == sizeof(uint64_t))
struct  compact_64b_set_bucket {
    uint32_t distance = {}; // 8 bytes, even if the distance is 1, we need to use 8 bytes to store it, so just use 8 bytes.
    union {
        Key key = {};
        uint64_t hash;
    };
};  // struct compact_long_set_bucket

static_assert(sizeof(compact_64b_set_bucket<uint64_t>) == 4 * sizeof(uint32_t), "compact_long_set_bucket should be 16 bytes");
static_assert(sizeof(compact_64b_set_bucket<int64_t>) == 4 * sizeof(uint32_t), "compact_long_set_bucket should be 16 bytes");

template<StringLike Key, typename Value>
struct compact_string_key_bucket {
    uint32_t distance = {};
    uint32_t hash = {}; // use 4 bytes hash is better than part of 8 bytes hash.
    Key key = {};
    Value value = {};
};  // struct compact_string_key_bucket

static_assert(sizeof(compact_string_key_bucket<memory::string, uint64_t>) == 5 * sizeof(uint64_t), "compact_long_hash should be 40 bytes");

template<StringLike Key, typename Value=void>
struct compact_string_set_bucket {
    uint32_t distance = {};
    uint32_t hash = {}; // use 4 bytes hash is better than part of 8 bytes hash.
    Key key = {};
};  // struct compact_string_set_bucket

static_assert(sizeof(compact_string_set_bucket<memory::string>) == 4 * sizeof(uint64_t), "compact_string_set_bucket should be 24 bytes");

// the loose means use 8 bytes to store the hash, 4 bytes to store the distance.
template <StringLike Key, typename Value, bool IsValueLong = (sizeof(Value) > sizeof(uint32_t))>
struct loose_string_key_bucket;

template <StringLike Key, typename Value>
struct loose_string_key_bucket<Key, Value, true>
{
    uint32_t distance = {}; // the Value has at least 8 bytes, so we can use 8 bytes to store the distance and hash.
    uint64_t hash = {};  // use 4 bytes to store the hash, to save 8 bytes of memory in each bucket.
    Key key = {};
    Value value = {}; // at least 8 bytes
};  // struct loose_string_key_bucket<Key, Value, true>

template <StringLike Key, typename Value>
struct loose_string_key_bucket<Key, Value, false>
{
    uint64_t hash = {}; // use 8 bytes to store the hash, make the risk of collision smaller.
    uint32_t distance = {}; // make the disntace to after the hash, and in front of the value field, so that we can save a 8 bytes in each bucket.
    Value value = {}; // at lea
    Key key = {};
};  // struct loose_string_key_bucket<Key, Value, false>

static_assert(sizeof(loose_string_key_bucket<memory::string, uint64_t>) == 6 * sizeof(uint64_t), "loose_string_key_bucket should be 32 bytes");
static_assert(sizeof(loose_string_key_bucket<memory::string, uint32_t>) == 5 * sizeof(uint64_t), "loose_string_key_bucket should be 32 bytes");

template<StringLike Key, typename Value = void>
struct loose_string_set_bucket {
    uint32_t distance = {}; // 8 bytes, even if the distance is 1, we need to use 8 bytes to store it, so just use 8 bytes.
    uint64_t hash = {}; // use 8 bytes to store the hash, make the risk of collision smaller.
    Key key = {};
};  // struct loose_string_set_bucket

static_assert(sizeof(loose_string_set_bucket<memory::string>) == 5 * sizeof(uint64_t), "loose_string_set_bucket should be 24 bytes");

}  // namespace bucket_type

template <typename Key, typename T, template <typename, typename> class Bucket, class Hash, class KeyEqual, template <typename> class Allocator>
class PlainHashMap
{
    using hash_type = decltype(Hash()(std::declval<Key>()));
    using fingerprint_type = uint64_t;
    static constexpr uint8_t initial_shifts = 64 - 2; // 2^(64-m_shift) number of buckets
    static constexpr float default_max_load_factor = 0.8F;

public:

    using bucket = Bucket<Key, T>;
    using bucket_ptr = Bucket<Key, T>*;
    using bucket_alloc = Allocator<bucket>;
    using bucket_place_type = uint32_t;
   private:
    bucket_alloc _bucket_alloc;
    bucket_ptr _buckets = nullptr;
    float _max_load_factor = default_max_load_factor;
    Hash _hasher;
    KeyEqual _equal;
    uint32_t _size = 0;
    uint32_t _num_buckets = 0;
    uint32_t _max_bucket_capacity = 0;
    uint8_t _shift = initial_shifts;

    [[nodiscard]] static constexpr auto max_bucket_count() -> uint64_t {
        return std::numeric_limits<uint32_t>::max();
    }

    [[nodiscard]] auto next(uint32_t bucket_place) -> uint32_t {
        return (bucket_place + 1U) % _num_buckets;
    }

    [[nodiscard]] static constexpr auto at(bucket_ptr bucket_ptr, uint32_t offset) -> bucket& {
        return bucket_ptr[offset];
    }

   private:
    // HASHING implementation
    [[gnu::always_inline, nodiscard]] auto do_hash(const Key& key) -> hash_type {
        return _hasher(key);
    }

    // calculate the bucket index from the hash
    [[nodiscard]] auto bucket_idx_from_hash(hash_type hash) -> bucket_place_type {
        return static_cast<uint32_t>(hash >> _shift);
    }

    [[nodiscard]] static constexpr auto get_key(bucket const& bucket) -> Key const& { return bucket.key; }

    [[nodiscard]] auto next_while_less(hash_type hash) const -> std::pair<bucket_place_type, bucket_place_type> {
        auto bucket_place = bucket_idx_from_hash(hash);
        auto distance = 0U;

        while (hash < at(_buckets, bucket_place).hash) {
            distance++;
            bucket_place = next(bucket_place);
        }

        return {distance, bucket_place};
    }

    void place_and_shift_up(bucket bucket, bucket_place_type place) {
        while (0 != at(_buckets, place).hash) {
            bucket = std::exchange(at(_buckets, place), std::move(bucket));
            bucket.distance++;
            place = next(place);
        }
        // the bucket's hash is 0, so it is an empty bucket.
        // no need to move the old bucket, just write the new bucket to the place.
        at(_buckets, place) = bucket;
    }

    [[nodiscard]] static constexpr auto calc_num_buckets(uint8_t shifts) -> uint64_t {
        return (std::min)(max_bucket_count(),
                          uint64_t{1} << (static_cast<uint64_t>(std::numeric_limits<uint64_t>::digits - shifts)));
    }

    [[nodiscard]] constexpr auto calc_shifts_for_size(uint64_t s) const -> uint8_t {
        auto shifts = initial_shifts;
        while (shifts > 0 && static_cast<uint64_t>(static_cast<float>(calc_num_buckets(shifts)) * _max_load_factor) < s) {
            --shifts;
        }
        return shifts;
    }

    // assumes m_values has data, m_buckets=m_buckets_end=nullptr, m_shifts is INITIAL_SHIFTS   
    void copy_buckets(PlainHashMap const& other) {
        // TODO(leo): implement it
        return;
    }

    [[nodiscard]] auto is_full() const -> bool {
        return _size > _max_bucket_capacity;
    }

    void deallocate_buckets() {
        if (nullptr != _buckets) {
            _bucket_alloc.deallocate(_buckets, _num_buckets);
            _buckets = nullptr;
        }
        _num_buckets = 0;
        _max_bucket_capacity = 0;
    }

    void allocate_buckets_from_shift() {
        _num_buckets = calc_num_buckets(_shift);
        _buckets = _bucket_alloc.allocate(_num_buckets);
        if (_num_buckets == max_bucket_count()) {
            // reached the maximum, make sure we can use each bucket
            _max_bucket_capacity = max_bucket_count();
        } else {
            _max_bucket_capacity = static_cast<uint32_t>(static_cast<float>(_num_buckets) * _max_load_factor);
        }
    }


    void clear_buckets() {
        // if Key is not trivial, we need to clear the keys.
        if constexpr (std::is_trivially_destructible_v<Key>) {
            // do nothing
        } else {
            for (uint32_t i = 0; i < _num_buckets; ++i) {
                auto& bucket = at(_buckets, i);
                bucket.key.~Key();
                if constexpr (not std::is_same_v<T, void> and not std::is_trivially_destructible_v<T>) {
                    bucket.value.~T();
                }
            }
        }
        // clear the buckets's memory layout.
        std::memset(_buckets, 0, sizeof(bucket) * _num_buckets);
    }

    void clear_and_fill_buckets_from_values() {
        clear_buckets();
        for (uint32_t i = 0; i < _num_buckets; ++i) {
            auto& bucket = at(_buckets, i);

            auto [dist_and_fingerprint, bucket_idx] = next_while_less(bucket.dist_and_fingerprint);
        }
    }
    
   public:
    [[nodiscard]] auto size() const -> uint64_t {
        return _size;
    }
};  // class PlainHashMap

} // namespace stdb::container
