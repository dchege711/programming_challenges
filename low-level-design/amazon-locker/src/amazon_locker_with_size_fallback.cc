#include "amazon_locker_with_size_fallback.h"

#include <ranges>
#include <vector>

#include "compartment.h"

namespace amazon_locker {

Compartment* AmazonLockerWithSizeFallback::find_available_compartment(
    Compartment::Size size) {
  const std::vector<Compartment::Size> fallback_order = {
      Compartment::Size::kSmall,
      Compartment::Size::kMedium,
      Compartment::Size::kLarge,
  };
  auto candidate_sizes =
      fallback_order |
      std::views::drop_while([size](Compartment::Size s) { return s != size; });

  for (Compartment::Size candidate_size : candidate_sizes) {
    auto* compartment =
        AmazonLocker::find_available_compartment(candidate_size);
    if (compartment != nullptr) {
      return compartment;
    }
  }
  return nullptr;
}

}  // namespace amazon_locker
