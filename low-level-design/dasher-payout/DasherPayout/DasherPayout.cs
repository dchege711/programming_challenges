using ExhaustiveMatching;

namespace DasherPayout;

public class DasherPayout
{
    public static decimal CalculatePayout(IEnumerable<External.DeliveryEvent> externalEvents, decimal baseRate, decimal bonusFactor) =>
        CalculatePayout(externalEvents.Select(e => e.ToInternal()), baseRate, bonusFactor);

    private static decimal CalculatePayout(IEnumerable<DeliveryEvent> events, decimal baseRate, decimal bonusFactor)
    {
        // Can't assume that events come in order. In a real system, network delays
        // might cause events to come out of order. Someone, e.g., our events store,
        // needs to guarantee that we're processing events in their correct order.
        IOrderedEnumerable<IGrouping<TimeOnly, DeliveryEvent>> sortedEvents =
            events.GroupBy(ev => ev.Time).OrderBy(g => g.Key);

        var concurrentCount = 0;
        Dictionary<string, int> multiplierByDeliveryId = [];
        decimal runningPayout = 0;

        HashSet<string> deliveryIdsPendingClosure = [];

        foreach (IGrouping<TimeOnly, DeliveryEvent> group in sortedEvents)
        {
            DeliveryEvent[] simultaneousEvents = [.. group];

            // Define `concurrentCount`'s delta independent of the original order in `events`.
            var newCount = group.Count(ev => ev.Status == DeliveryStatus.Accepted);
            concurrentCount += newCount;
            concurrentCount -= simultaneousEvents.Length - newCount;

            foreach (DeliveryEvent @event in simultaneousEvents)
            {
                switch (@event.Status)
                {
                    case DeliveryStatus.Accepted:
                        {
                            var added = deliveryIdsPendingClosure.Add(@event.Id);
                            if (!added)
                            {
                                throw new ArgumentException($"Multiple Accepted events for delivery {@event.Id}");
                            }

                            multiplierByDeliveryId[@event.Id] = concurrentCount;
                            break;
                        }

                    case DeliveryStatus.Delivered:
                        {
                            var removed = deliveryIdsPendingClosure.Remove(@event.Id);
                            if (!removed)
                            {
                                throw new ArgumentException($"Delivery {@event.Id} delivered w/o being accepted first.");
                            }

                            runningPayout += baseRate * (1 + (bonusFactor * multiplierByDeliveryId[@event.Id]));

                            break;
                        }

                    case DeliveryStatus.Cancelled:
                        {
                            var removed = deliveryIdsPendingClosure.Remove(@event.Id);
                            if (!removed)
                            {
                                throw new ArgumentException($"Delivery {@event.Id} cancelled w/o being accepted first.");
                            }

                            break;
                        }

                    default:
                        {
                            throw ExhaustiveMatch.Failed(@event.Status);
                        }
                }
            }
        }

        return runningPayout;
    }
}
