namespace ConnectFour;

public class GameWithUndo(Player player1, Player player2) : Game(player1, player2)
{
    public override Coordinate PlaceDisc(Player player, int column)
    {
        var destination = base.PlaceDisc(player, column);
        movesHistory.Push(new(player, destination));
        return destination;
    }

    public bool Undo()
    {
        if (GetState() != State.InProgress)
            throw new InvalidOperationException("Undo is only supported for in-progress games.");

        if (!movesHistory.TryPop(out var lastMove))
            return false;

        _board.Clear(lastMove.Coordinate);
        _currentPlayer = lastMove.Player;
        return true;
    }

    record struct Move(Player Player, Coordinate Coordinate);

    private Stack<Move> movesHistory = [];
}
