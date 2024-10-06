#include <cstdint>
#include <utility>
#include <limits>
#include <algorithm>
#include <cstring>

namespace stdb::container {

template <typename Key, typename T, class Hash, class KeyEqual, template <typename> class Allocator>
class PlainHashMap
{
    static constexpr uint8_t initial_shifts = 64 - 2; // 2^(64-m_shift) number of buckets
    static constexpr float default_max_load_factor = 0.8F;

    struct Bucket_without_value {
        static constexpr uint64_t dist_inc = 1U << 8U;
        static constexpr uint64_t fingerprint_mask = dist_inc - 1;
        uint64_t dist_and_fingerprint{};
        Key key;
    };
    struct Bucket_with_value : Bucket_without_value {
        T value;
    };

    using Bucket = std::conditional_t<std::is_same_v<T, void>, Bucket_without_value, Bucket_with_value>;
    using bucket_ptr = Bucket*;
    using bucket_alloc = Allocator<Bucket>;
    using bucket_idx_type = uint32_t;
    using hash_type = decltype(mixed_hash(std::declval<Key>()));
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
    using dist_and_fingerprint_type = decltype(Bucket::dist_and_fingerprint);


    [[nodiscard]] static constexpr auto max_bucket_count() -> uint64_t {
        return std::numeric_limits<uint32_t>::max();
    }

    [[nodiscard]] auto next(uint32_t bucket_idx) -> uint32_t {
        return (bucket_idx + 1U) % _num_buckets;
    }

    [[nodiscard]] static constexpr auto at(bucket_ptr bucket_ptr, uint32_t offset) -> Bucket& {
        return bucket_ptr[offset];
    }

    [[nodiscard]] static constexpr auto dist_inc(dist_and_fingerprint_type fingerprint) -> uint64_t {
        return fingerprint + Bucket::dist_inc;
    }

    [[nodiscard]] static constexpr auto dist_dec(dist_and_fingerprint_type fingerprint) -> uint64_t {
        return fingerprint - Bucket::dist_inc;
    }

   private:
    // HASHING implementation
    [[gnu::always_inline, nodiscard]] auto mixed_hash(const Key& key) -> hash_type {
        return _hasher(key);
    }

    [[gnu::always_inline, nodiscard]] auto mixed_hash(Key key) -> hash_type { return _hasher(key); }

    [[nodiscard]] auto dist_and_fingerprint_from_hash(hash_type hash) -> dist_and_fingerprint_type {
        return Bucket::dist_inc | (static_cast<dist_and_fingerprint_type>(hash) & Bucket::fingerprint_mask);
    }

    [[nodiscard]] auto bucket_idx_from_hash(hash_type hash) -> bucket_idx_type {
        return static_cast<uint32_t>(hash >> _shift);
    }

    [[nodiscard]] static constexpr auto get_key(Bucket const& bucket) -> Key const& { return bucket.key; }

    [[nodiscard]] auto next_while_less(hash_type hash) const -> dist_and_fingerprint_type {
        auto dist_and_fingprint = dist_and_fingerprint_from_hash(hash);
        auto bucket_idx = bucket_idx_from_hash(hash);

        while (dist_and_fingprint < at(_buckets, bucket_idx).dist_and_fingerprint) {
            dist_and_fingprint = dist_inc(dist_and_fingprint);
            bucket_idx = next(bucket_idx);
        }

        return {dist_and_fingprint, bucket_idx};
    }

    void place_and_shift_up(Bucket bucket, bucket_idx_type place) {
        while (0 != at(_buckets, place).dist_and_fingerprint) {
            bucket = std::exchange(at(_buckets, place), std::move(bucket));
            bucket.dist_and_fingerprint = dist_inc(bucket.dist_and_fingerprint);
            place = next(place);
        }
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
        std::memset(_buckets, 0, sizeof(Bucket) * _num_buckets);
    }

    void clear_and_fill_buckets_from_values() {
        clear_buckets();
        for (uint32_t i = 0; i < _num_buckets; ++i) {
            auto& bucket = at(_buckets, i);

            
        }
    }
    
   public:
    [[nodiscard]] auto size() const -> uint64_t {
        return _size;
    }
};  // class PlainHashMap

} // namespace stdb::container
