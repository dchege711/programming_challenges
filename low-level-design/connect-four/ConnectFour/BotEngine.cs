namespace ConnectFour;

public interface IBotEngine
{
    int ChooseColumn(Game game);
}

public class FirstAvailableSpotBotEngine : IBotEngine
{
    public int ChooseColumn(Game game)
    {
        var board = game.GetBoard();
        for (int c = 0; c < board.GetColCount(); c++)
        {
            if (board.CanPlaceDisc(c))
                return c;
        }

        throw new InvalidOperationException("No spots available on board.");
    }
}

public class LastAvailableSpotBotEngine : IBotEngine
{
    public int ChooseColumn(Game game)
    {
        var board = game.GetBoard();
        for (int c = board.GetColCount() - 1; c >= 0; c--)
        {
            if (board.CanPlaceDisc(c))
                return c;
        }

        throw new InvalidOperationException("No spots available on board.");
    }
}
