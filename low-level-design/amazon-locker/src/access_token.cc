#include "access_token.h"

namespace amazon_locker {

namespace {
constexpr std::chrono::duration kAllowedPickupTime = std::chrono::days(7);
}  // namespace

AccessToken::AccessToken(std::string access_code, Compartment& compartment)
    : access_code_(std::move(access_code)),
      compartment_(compartment),
      expiry_date_(std::chrono::system_clock::now() + kAllowedPickupTime) {}

}  // namespace amazon_locker
