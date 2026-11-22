#ifndef ACCESS_TOKEN_H
#define ACCESS_TOKEN_H

#include <chrono>
#include <string>

#include "compartment.h"

namespace amazon_locker {
class AccessToken {
 public:
  AccessToken(std::string access_code, Compartment& compartment);

  bool is_expired() const {
    return std::chrono::system_clock::now() > expiry_date_;
  }

  const std::string& access_code() const { return access_code_; }
  Compartment& compartment() { return compartment_; }

 private:
  Compartment& compartment_;
  std::string access_code_;
  const std::chrono::time_point<std::chrono::system_clock> expiry_date_;
};
}  // namespace amazon_locker

#endif  // ACCESS_TOKEN_H
