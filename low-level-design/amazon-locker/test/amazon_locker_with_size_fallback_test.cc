#include "amazon_locker_with_size_fallback.h"

#include <gtest/gtest.h>

#include <string>

using amazon_locker::AmazonLockerWithSizeFallback;
using amazon_locker::Compartment;

TEST(AmazonLockerWithSizeFallback, FitsSmallIntoMediumAndLarge) {
  AmazonLockerWithSizeFallback locker({.small = 1, .medium = 1, .large = 1});

  auto access_code_1 = locker.deposit_package(Compartment::Size::kSmall);
  auto access_code_2 = locker.deposit_package(Compartment::Size::kSmall);
  auto access_code_3 = locker.deposit_package(Compartment::Size::kSmall);

  EXPECT_TRUE(access_code_1.has_value());
  EXPECT_TRUE(access_code_2.has_value());
  EXPECT_TRUE(access_code_3.has_value());
}
