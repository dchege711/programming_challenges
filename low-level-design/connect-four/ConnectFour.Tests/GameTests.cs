namespace ConnectFour.Tests;

[TestClass]
public sealed class GameTests
{
    [TestMethod]
    public void PlayGame_ToCompletion_Succeeds()
    {
        // Arrange
        Player alice = new("Alice", DiscColor.Red);
        Player bob = new("Bob", DiscColor.Yellow);
        Game game = new(alice, bob);

        // Act: Have Alice win the game
        for (int i = 0; i < 3; i++)
        {
            game.PlaceDisc(alice, 0);
            game.PlaceDisc(bob, 1);
        }
        game.PlaceDisc(alice, 0);

        // Assert
        Assert.AreEqual(Game.State.Won, game.GetState());
        Assert.AreEqual(alice, game.Winner());
    }

    [TestMethod]
    public void PlayGame_Partially_Succeeds()
    {
        // Arrange
        Player alice = new("Alice", DiscColor.Red);
        Player bob = new("Bob", DiscColor.Yellow);
        Game game = new(alice, bob);

        // Act: Have Alice and Bob play two rounds.
        for (int i = 0; i < 2; i++)
        {
            game.PlaceDisc(alice, 0);
            game.PlaceDisc(bob, 1);
        }

        // Assert
        Assert.AreEqual(Game.State.InProgress, game.GetState());
        Assert.IsNull(game.Winner());
        Assert.AreEqual(alice, game.CurrentPlayer());
        Assert.Throws<InvalidOperationException>(() => game.PlaceDisc(bob, 1));
    }

    [TestMethod]
    public void PlayGame_Undo_Succeeds()
    {
        // Arrange
        Player alice = new("Alice", DiscColor.Red);
        Player bob = new("Bob", DiscColor.Yellow);
        GameWithUndo game = new(alice, bob);

        for (int i = 0; i < 2; i++)
        {
            game.PlaceDisc(alice, 0);
            game.PlaceDisc(bob, 1);
        }
        Assert.AreEqual(alice, game.CurrentPlayer());

        // Act: Undo Bob's last move.
        game.Undo();

        // Assert
        Assert.AreEqual(Game.State.InProgress, game.GetState());
        Assert.AreEqual(bob, game.CurrentPlayer());
    }
}
