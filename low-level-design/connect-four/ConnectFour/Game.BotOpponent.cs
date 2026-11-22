namespace ConnectFour;

public partial class Game
{
    public Board GetBoard() => new(_board);
}
