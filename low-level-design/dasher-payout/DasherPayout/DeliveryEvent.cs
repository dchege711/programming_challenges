namespace DasherPayout;

internal enum DeliveryStatus { Accepted, Delivered, Cancelled }

internal record struct DeliveryEvent(string Id, DeliveryStatus Status, TimeOnly Time);
