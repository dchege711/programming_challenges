using FluentAssertions;

namespace DasherPayout.Tests;

[TestClass]
public sealed class DasherPayoutTests
{
    [TestMethod]
    public void SingleDelivery_Succeeds()
    {
        List<External.DeliveryEvent> deliveryEvents = [
            new("D1", "accepted", "10:00"),
            new("D1", "delivered", "10:30"),
        ];
        const int baseRate = 10;
        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate);
        payout.Should().Be(10);
    }

    [TestMethod]
    public void MultipleNonOverlappingDeliveries_Succeeds()
    {
        List<External.DeliveryEvent> deliveryEvents = [
            new("D1", "accepted", "10:00"),
            new("D1", "delivered", "10:30"),
            new("D2", "accepted", "11:00"),
            new("D2", "delivered", "11:30"),
            new("D3", "accepted", "12:00"),
            new("D3", "delivered", "12:30"),
        ];
        const int baseRate = 10;
        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate);
        payout.Should().Be(10 + 10 + 10);
    }

    [TestMethod]
    public void AllOverlappingDeliveries_Succeeds()
    {
        List<External.DeliveryEvent> deliveryEvents = [
            new("D1", "accepted", "10:00"),
            new("D1", "delivered", "14:00"),
            new("D2", "accepted", "10:30"),
            new("D2", "delivered", "14:00"),
            new("D3", "accepted", "11:00"),
            new("D3", "delivered", "11:30"),
        ];
        const int baseRate = 10;
        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate);
        payout.Should().Be(10 + 20 + 30);
    }
}
