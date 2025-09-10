using System.Collections;

namespace AoC2024;

public partial class GuardGallivant
{
    public int PartOne() =>
        SimulateGuardMoves(startingPosition.r, startingPosition.c);

    private int SimulateGuardMoves(int r, int c)
    {
        var orientation = ToOrientation(areaMap[r, c].Visits);
        var (dr, dc) = ToVector(orientation);
        var (nr, nc) = (r + dr, c + dc);
        int numVisitedPositions = 1;

        while (InBounds(nr, nc))
        {
            var (isBlocked, visits) = areaMap[nr, nc];

            if (isBlocked)
            {
                // Backtrack and re-orient.
                (nr, nc) = (nr - dr, nc - dc);
                orientation = TurnRight90Degrees(orientation);
                (dr, dc) = ToVector(orientation);
            }
            else if (visits?.HasAnySet() == false)
            {
                // First time visiting this location.
                numVisitedPositions += 1;
                visits.Set((int)orientation, true);
            }

            (nr, nc) = (nr + dr, nc + dc);
        }

        return numVisitedPositions;
    }

    private static Orientation TurnRight90Degrees(Orientation orientation) => orientation switch {
        Orientation.Up => Orientation.Right,
        Orientation.Right => Orientation.Down,
        Orientation.Down => Orientation.Left,
        Orientation.Left => Orientation.Up,
        _ => throw new ArgumentException($"Unrecognized input: {orientation}")
    };

    private static (int dr, int dc) ToVector(Orientation orientation) => orientation switch {
        Orientation.Up => (-1, 0),
        Orientation.Right => (0, 1),
        Orientation.Down => (1, 0),
        Orientation.Left => (0, -1),
        _ => throw new ArgumentException($"Unrecognized input: {orientation}")
    };

    private static Orientation ToOrientation(BitArray? visits)
    {
        if (visits is null)
            throw new ArgumentException("Invalid visits record (null)");
        
        var (numBitsSet, idxSetBit) = visits.Cast<bool>()
            .Index()
            .Aggregate((0, 0), (acc, val) => val.Item ? (acc.Item1 + 1, val.Index) : acc);

        if (numBitsSet != 1)
            throw new ArgumentException($"Invalid visits record {visits}: numBitsSet={numBitsSet}");

        return (Orientation)idxSetBit;
    }

    private static Orientation ToOrientation(int dr, int dc) => (dr, dc) switch {
        { dr: > 0, dc: 0 } => Orientation.Down,
        { dr: 0, dc: < 0 } => Orientation.Left,
        { dr: < 0, dc: 0 } => Orientation.Up,
        { dr: 0, dc: > 0 } => Orientation.Right,
        _ => throw new ArgumentException("Invalid direction vector")
    };

    private bool InBounds(int r, int c) =>
        r >= 0 && r < areaMap.GetLength(0) && c >= 0 && c < areaMap.GetLength(1);
}
