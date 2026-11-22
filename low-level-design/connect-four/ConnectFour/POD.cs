namespace ConnectFour;

public enum DiscColor { Red, Yellow }

public record struct Player(string Name, DiscColor DiscColor);

public record struct Delta(int dR, int dC)
{
    public static Delta operator *(Delta delta, int factor) =>
        new(delta.dR * factor, delta.dC * factor);
}

public record struct Coordinate(int R, int C)
{
    public static Coordinate operator +(Coordinate coord, Delta delta) =>
        new(coord.R + delta.dR, coord.C + delta.dC);
}
