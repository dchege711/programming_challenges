using ExhaustiveMatching;

namespace DasherPayout;

public class DasherPayout
{
    public static int CalculatePayout(IEnumerable<External.DeliveryEvent> externalEvents, int baseRate) =>
        CalculatePayout(externalEvents.Select(e => e.ToInternal()), baseRate);

    private static int CalculatePayout(IEnumerable<DeliveryEvent> events, int baseRate)
    {
        // Can't assume that events come in order. In a real system, network delays
        // might cause events to come out of order. Someone, e.g., our events store,
        // needs to guarantee that we're processing events in their correct order.
        IOrderedEnumerable<DeliveryEvent> sortedEvents = events.OrderBy(ev => ev.Time);

        var concurrentCount = 0;
        Dictionary<string, int> multiplierByDeliveryId = [];
        var runningPayout = 0;

        HashSet<string> deliveryIdsPendingClosure = [];

        foreach (DeliveryEvent @event in sortedEvents)
        {
            switch (@event.Status)
            {
                case DeliveryStatus.Accepted:
                    {
                        concurrentCount += 1;
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

                        runningPayout += multiplierByDeliveryId[@event.Id] * baseRate;
                        concurrentCount -= 1;

                        break;
                    }

                case DeliveryStatus.Cancelled:
                    {
                        var removed = deliveryIdsPendingClosure.Remove(@event.Id);
                        if (!removed)
                        {
                            throw new ArgumentException($"Delivery {@event.Id} cancelled w/o being accepted first.");
                        }

                        concurrentCount -= 1;

                        break;
                    }

                default:
                    {
                        throw ExhaustiveMatch.Failed(@event.Status);
                    }
            }
        }

        return runningPayout;
    }
}
