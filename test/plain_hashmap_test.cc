#include "container/plain_hashmap.hpp"
#include "string/string.hpp"
#include "doctest/doctest.h"


namespace stdb::container {

using plain_hashmap = PlainHashMap<stdb::memory::string, uint32_t, std::hash<stdb::memory::string>,
                                   std::equal_to<stdb::memory::string>,
                                   std::allocator>;
static_assert(sizeof(plain_hashmap::Bucket_without_value) == 40, "Bucket_without_value should be 32 bytes");
static_assert(sizeof(plain_hashmap::Bucket) == 40, "Bucket_without_value should be 40 bytes");

TEST_CASE("PlainHashMap::size") {
    plain_hashmap map;
    CHECK_EQ(map.size(), 0);
}

} // namespace stdb::container


