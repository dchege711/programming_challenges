using System.Globalization;

namespace DasherPayout;

internal static class Converters
{
    extension(External.DeliveryEvent deliveryEvent)
    {
        internal DeliveryEvent ToInternal()
        {
            DeliveryStatus status = Enum.Parse<DeliveryStatus>(deliveryEvent.Status, ignoreCase: true);
            if (!Enum.IsDefined(status))
            {
                throw new ArgumentException($"{status} is not a recognized event status");
            }

            var time = TimeOnly.Parse(deliveryEvent.Time, CultureInfo.InvariantCulture);
            return new(deliveryEvent.Id, status, time);
        }
    }
}
