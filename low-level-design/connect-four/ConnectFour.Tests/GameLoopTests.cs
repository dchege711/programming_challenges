namespace ConnectFour.Tests;

[TestClass]
public sealed class GameLoopTests
{
    [TestMethod]
    public void GameLoop_Completes()
    {
        // Arrange
        Player player1 = new("Vision", DiscColor.Red);
        Player player2 = new("Ultron", DiscColor.Yellow);
        Game game = new(player1, player2);

        IBotEngine engine1 = new FirstAvailableSpotBotEngine();
        IBotEngine engine2 = new LastAvailableSpotBotEngine();

        // Act: Have each bot take turns.
        while (game.GetState() == Game.State.InProgress)
        {
            var player = game.CurrentPlayer();
            var column = player == player1
                ? engine1.ChooseColumn(game)
                : engine2.ChooseColumn(game);
            game.PlaceDisc(player, column);
        }

        // Assert.
        Assert.AreEqual(player1, game.Winner());
    }
}
