#ifndef COMPARTMENT_H_
#define COMPARTMENT_H_

#include <cstdint>

namespace amazon_locker {

class Compartment {
 public:
  enum class State : std::uint8_t { kAvailable, kOccupied };

  enum class Size : std::uint8_t { kSmall, kMedium, kLarge };

  explicit Compartment(Size size);

  void mark_available() { state_ = State::kAvailable; }

  void mark_occupied() { state_ = State::kOccupied; }

  bool is_available() const { return state_ == State::kAvailable; }

  Size size() const { return size_; }

 private:
  State state_ = State::kAvailable;
  const Size size_;
};

}  // namespace amazon_locker

#endif  // COMPARTMENT_H_
