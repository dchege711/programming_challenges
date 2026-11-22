#ifndef AMAZON_LOCKER_H
#define AMAZON_LOCKER_H

#include <cstddef>
#include <expected>
#include <map>
#include <string>
#include <vector>

#include "access_token.h"
#include "compartment.h"

namespace amazon_locker {

class AmazonLocker {
 public:
  struct CompartmentCounts {
    std::size_t small = 0;
    std::size_t medium = 0;
    std::size_t large = 0;
  };

  explicit AmazonLocker(CompartmentCounts counts);
  virtual ~AmazonLocker() = default;

  enum class DepositError : std::uint8_t { kNoAvailableCompartments };
  std::expected<std::string, DepositError> deposit_package(
      Compartment::Size size);

  enum class RetrievalError : std::uint8_t { kInvalidCode, kExpiredCode };
  std::optional<RetrievalError> retrieve_package(
      const std::string& access_code);

  void open_all_expired_compartments();

 protected:
  // Made virtual for one of the extensibility exercises.
  virtual Compartment* find_available_compartment(Compartment::Size size);

 private:
  void mark_available(AccessToken& access_token);

  std::vector<Compartment> compartments_;

  std::map<std::string, AccessToken> access_tokens_by_access_code_;
};

}  // namespace amazon_locker

#endif  // AMAZON_LOCKER_H
