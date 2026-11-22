#include "amazon_locker.h"

#include <algorithm>
#include <expected>
#include <optional>
#include <random>
#include <ranges>
#include <string_view>

#include "access_token.h"
#include "compartment.h"

namespace amazon_locker {

namespace {

std::string generate_access_code() {
  constexpr std::string_view charset = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  constexpr size_t code_length = 6;

  // Set up thread-safe, non-deterministic random engine and distribution.
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<std::size_t> dist(0, charset.size() - 1);

  // Transform a sequence of random numbers into a string
  return std::views::repeat(0) |
         std::views::transform([&](int) { return charset[dist(gen)]; }) |
         std::views::take(code_length) | std::ranges::to<std::string>();
}

}  // namespace

AmazonLocker::AmazonLocker(CompartmentCounts counts) {
  for (size_t i = 0; i < counts.small; i++) {
    compartments_.emplace_back(Compartment::Size::kSmall);
  }
  for (size_t i = 0; i < counts.medium; i++) {
    compartments_.emplace_back(Compartment::Size::kMedium);
  }
  for (size_t i = 0; i < counts.large; i++) {
    compartments_.emplace_back(Compartment::Size::kLarge);
  }
}

std::expected<std::string, AmazonLocker::DepositError>
AmazonLocker::deposit_package(Compartment::Size size) {
  auto* compartment = find_available_compartment(size);
  if (compartment == nullptr) {
    return std::unexpected<AmazonLocker::DepositError>(
        AmazonLocker::DepositError::kNoAvailableCompartments);
  }

  std::string access_code;
  do {
    access_code = generate_access_code();
  } while (access_tokens_by_access_code_.contains(access_code));

  AccessToken access_token(access_code, *compartment);
  access_tokens_by_access_code_.emplace(access_code, std::move(access_token));
  compartment->mark_occupied();

  return access_code;
}

std::optional<AmazonLocker::RetrievalError> AmazonLocker::retrieve_package(
    const std::string& access_code) {
  if (access_code.empty()) {
    return AmazonLocker::RetrievalError::kInvalidCode;
  }

  auto it = access_tokens_by_access_code_.find(access_code);
  if (it == access_tokens_by_access_code_.end()) {
    return AmazonLocker::RetrievalError::kInvalidCode;
  }

  auto& access_token = it->second;
  if (access_token.is_expired()) {
    return AmazonLocker::RetrievalError::kExpiredCode;
  }

  mark_available(access_token);
  return std::nullopt;
}

void AmazonLocker::open_all_expired_compartments() {
  std::vector<AccessToken*> expired_access_tokens;
  for (auto& access_token : std::views::values(access_tokens_by_access_code_)) {
    if (access_token.is_expired()) {
      expired_access_tokens.push_back(&access_token);
    }
  }

  for (auto* access_token : expired_access_tokens) {
    mark_available(*access_token);
  }
}

Compartment* AmazonLocker::find_available_compartment(Compartment::Size size) {
  auto is_available = [size](Compartment& compartment) {
    return compartment.size() == size && compartment.is_available();
  };
  auto match = std::ranges::find_if(compartments_, is_available);
  return match == compartments_.end() ? nullptr : &*match;
}

void AmazonLocker::mark_available(AccessToken& access_token) {
  access_token.compartment().mark_available();
  access_tokens_by_access_code_.erase(access_token.access_code());
}

}  // namespace amazon_locker
