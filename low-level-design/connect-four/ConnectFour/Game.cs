namespace ConnectFour;

public partial class Game(Player _player1, Player _player2)
{
    public enum State { InProgress, Won, Drawn }

    public Player CurrentPlayer() => _currentPlayer;

    // Made virtual for extensibility, see GameWithUndo.
    public virtual Coordinate PlaceDisc(Player player, int column)
    {
        if (GetState() != State.InProgress)
            throw new InvalidOperationException($"No moves allowed when Game is in the {GetState()} state.");

        if (CurrentPlayer() != player)
            throw new InvalidOperationException($"It's not {player.Name}'s turn");

        if (!_board.CanPlaceDisc(column))
            throw new InvalidOperationException($"No disc can be dropped into {column}");

        var restingSpot = _board.PlaceDisc(player.DiscColor, column);

        if (_board.CheckWin(restingSpot)) { _gameState = State.Won; }
        else if (_board.IsFull()) { _gameState = State.Drawn; }
        else { _currentPlayer = _currentPlayer == _player1 ? _player2 : _player1; }

        return restingSpot;
    }

    public State GetState() => _gameState;

    public Player? Winner() => _gameState == State.Won ? _currentPlayer : null;

    protected Player _currentPlayer = _player1;
    protected Board _board = new();

    protected State _gameState = State.InProgress;
}
