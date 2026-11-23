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

        const decimal baseRate = 10;
        const decimal bonusFactor = 0.25M;

        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate, bonusFactor);
        payout.Should().Be(baseRate * (1 + (bonusFactor * 1)));
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

        const decimal baseRate = 10;
        const decimal bonusFactor = 0.25M;

        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate, bonusFactor);
        payout.Should().Be(
            (baseRate * (1 + (bonusFactor * 1)))
            + (baseRate * (1 + (bonusFactor * 1)))
            + (baseRate * (1 + (bonusFactor * 1))));
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

        const decimal baseRate = 10;
        const decimal bonusFactor = 0.25M;

        var payout = DasherPayout.CalculatePayout(deliveryEvents, baseRate, bonusFactor);
        payout.Should().Be(
            (baseRate * (1 + (bonusFactor * 1)))
            + (baseRate * (1 + (bonusFactor * 2)))
            + (baseRate * (1 + (bonusFactor * 3))));
    }
}
