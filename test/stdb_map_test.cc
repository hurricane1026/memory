#include "container/stdb_map.hpp"
#include "doctest/doctest.h"
#include <vector>
#include <string>

namespace stdb::container {

static_assert(IsStringView<std::string_view>);
static_assert(!IsStringView<std::string>);
static_assert(!IsStringView<stdb::memory::string>);
static_assert(IsString<stdb::memory::string>);
static_assert(IsString<std::string>);
static_assert(IsString<std::u32string>);
static_assert(IsString<std::vector<char>>);



} // namespace stdb::container