#include "amazon_locker.h"

#include <gtest/gtest.h>

#include <string>

using amazon_locker::AmazonLocker;
using amazon_locker::Compartment;

TEST(AmazonLockerTest, DepositAndPickup) {
  AmazonLocker locker({.small = 1, .medium = 1, .large = 1});

  auto access_code = locker.deposit_package(Compartment::Size::kSmall);
  EXPECT_TRUE(access_code.has_value());

  auto error = locker.retrieve_package(access_code.value());
  EXPECT_FALSE(error.has_value());
}

TEST(AmazonLockerTest, ReuseLockerAfterRetrieval) {
  AmazonLocker locker({.small = 1, .medium = 1, .large = 1});

  auto access_code_1 = locker.deposit_package(Compartment::Size::kMedium);
  auto access_code_2 = locker.deposit_package(Compartment::Size::kMedium);

  EXPECT_EQ(access_code_2.error(),
            AmazonLocker::DepositError::kNoAvailableCompartments);

  locker.retrieve_package(access_code_1.value());
  access_code_2 = locker.deposit_package(Compartment::Size::kMedium);

  EXPECT_TRUE(access_code_2.has_value());
}

TEST(AmazonLockerTest, NoCompartments) {
  AmazonLocker locker({.small = 0, .medium = 0, .large = 0});

  auto access_code = locker.deposit_package(Compartment::Size::kLarge);
  EXPECT_FALSE(access_code.has_value());
  EXPECT_EQ(access_code.error(),
            AmazonLocker::DepositError::kNoAvailableCompartments);
}

TEST(AmazonLockerTest, CompartmentCountsBySize) {
  AmazonLocker locker({.medium = 1, .large = 2});

  EXPECT_FALSE(locker.deposit_package(Compartment::Size::kSmall).has_value());
  EXPECT_TRUE(locker.deposit_package(Compartment::Size::kMedium).has_value());
  EXPECT_FALSE(locker.deposit_package(Compartment::Size::kMedium).has_value());
  EXPECT_TRUE(locker.deposit_package(Compartment::Size::kLarge).has_value());
  EXPECT_TRUE(locker.deposit_package(Compartment::Size::kLarge).has_value());
  EXPECT_FALSE(locker.deposit_package(Compartment::Size::kLarge).has_value());
}
