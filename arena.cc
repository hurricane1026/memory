/*
 * Copyright (C) 2020 hurricane <l@stdb.io>. All rights reserved.
 */
/* +------------------------------------------------------------------+
   |                                                                  |
   |                                                                  |
   |  #####                                                           |
   | #     # #####   ##   #####  #####  # #    # ######  ####   ####  |
   | #         #    #  #  #    # #    # # ##   # #      #      #      |
   |  #####    #   #    # #    # #    # # # #  # #####   ####   ####  |
   |       #   #   ###### #####  #####  # #  # # #           #      # |
   | #     #   #   #    # #   #  #   #  # #   ## #      #    # #    # |
   |  #####    #   #    # #    # #    # # #    # ######  ####   ####  |
   |                                                                  |
   |                                                                  |
   +------------------------------------------------------------------+
*/

#include "arena.hpp"

#include <algorithm>
#include <limits>

using stdb::memory::Arena;

Arena::Block::Block(uint64_t size, Block* prev)
    : prev_(prev), pos_(kBlockHeaderSize), size_(size) {}

// generate a new memory Block.
Arena::Block* Arena::NewBlock(uint64_t min_bytes, Block* prev_block) {
  uint64_t required_bytes = min_bytes + kBlockHeaderSize;
  uint64_t size = 0;

  // verify not overflow, with glog
  CHECK_LE(min_bytes, std::numeric_limits<uint64_t>::max() - kBlockHeaderSize);

  if (prev_block != nullptr) {
    // not the first block "New" action.
    if (required_bytes <= options_.normal_block_size) {
      size = options_.normal_block_size;
    } else if (required_bytes <= options_.huge_block_size / kThresholdHuge) {
      size = align::AlignUp(min_bytes, options_.normal_block_size);
    } else if ((required_bytes > options_.huge_block_size / kThresholdHuge) &&
               (required_bytes <= options_.huge_block_size)) {
      size = options_.huge_block_size;
    }
    // for the more than huge_block_size size
    // will be handle out of the code scope
    // by now, the size remains to be 0.
  } else {
    // the size may be insuffcient than the required.
    size = options_.suggested_initblock_size;
  }

  // NOTICE: when the size will be change to required_bytes?
  // #1. larger than huge on second or older block.
  // #2. larger than suggested_initblock_size on first block.
  // on both of them, the block will be monoplolized.
  //
  // if size is insuffcient, make it suffcient
  size = std::max(size, required_bytes);

  // allocate the memory by the block_alloc function.
  // no AlignUpTo8 need, because
  // normal_block_size and huge_block_size should be power of 2.
  // if the size over the huge_block_size, the block will be monoplolized.
  void* mem = options_.block_alloc(size);
  // if mem == nullptr, means no memory available for current os status.
  if (mem == nullptr) return nullptr;
  // if mem == nullptr, placement new will trigger a segmentfault
  Block* b = new (mem) Block(size, prev_block);
  space_allocated_ += size;
  return b;
}

// if return nullptr means failure
char* Arena::allocateAligned(uint64_t bytes) {
  uint64_t needed = align_size(bytes);
  if (last_block_ == nullptr || needed > last_block_->remain()) {
    Block* curr = NewBlock(needed, last_block_);
    if (curr != nullptr)
      last_block_ = curr;
    else
      return nullptr;
  }
  char* result = last_block_->alloc(needed);
  // make sure aligned
  assert((reinterpret_cast<uint64_t>(result) & 7) == 0);
  return result;
}
