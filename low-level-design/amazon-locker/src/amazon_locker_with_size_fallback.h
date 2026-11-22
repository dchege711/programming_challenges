#ifndef AMAZON_LOCKER_WITH_SIZE_FALLBACK_H_
#define AMAZON_LOCKER_WITH_SIZE_FALLBACK_H_

#include "amazon_locker.h"
#include "compartment.h"

namespace amazon_locker {

class AmazonLockerWithSizeFallback : public AmazonLocker {
 public:
  using AmazonLocker::AmazonLocker;

 protected:
  Compartment* find_available_compartment(Compartment::Size size) override;
};

}  // namespace amazon_locker

#endif  // AMAZON_LOCKER_WITH_SIZE_FALLBACK_H_
