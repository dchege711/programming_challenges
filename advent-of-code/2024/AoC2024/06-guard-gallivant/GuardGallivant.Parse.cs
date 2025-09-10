using System.Collections;

namespace AoC2024;

public partial class GuardGallivant
{
    public readonly (bool Blocked, BitArray Visits)[,] areaMap;
    public readonly (int r, int c) startingPosition;

    public enum PositionState { kBlocked, kVisited, kUnVisited }

    public enum Orientation { Up, Down, Left, Right }

    public GuardGallivant(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        List<char[]> rows = [];

        string? line;
        while((line = inputReader.ReadLine()) is not null)
            rows.Add(line.ToCharArray());
        
        int rowCount = rows.Count;
        int colCount = rows.First().Length;

        areaMap = new (bool Blocked, BitArray Visits)[rowCount, colCount];
        for (int r = 0; r < rowCount; r++)
        {
            foreach (var (c, val) in rows[r].Index())
            {
                var positionState = GetPositionState(val);
                areaMap[r, c] = positionState;

                if (positionState.Visits?.HasAnySet() == true)
                    startingPosition = (r, c);
            }
        }
    }

    private static Orientation GetOrientation(char val) => val switch {
        '^' => Orientation.Up,
        '<' => Orientation.Left,
        '>' => Orientation.Right,
        'v' => Orientation.Down,
        _ => throw new ArgumentException($"Unrecognized input: {val}")
    };

    private static (bool Blocked, BitArray Visits) GetPositionState(char c) => c switch {
        '.' => (false, new BitArray(4)),
        '#' => (true, new BitArray(4)),
        '^' => (false, GetBitArray(Orientation.Up)),
        '<' => (false, GetBitArray(Orientation.Left)),
        '>' => (false, GetBitArray(Orientation.Right)),
        'v' => (false, GetBitArray(Orientation.Down)),
        _ => throw new ArgumentException($"Unrecognized input: {c}")
    };

    private static BitArray GetBitArray(Orientation orientation) =>
        new(
            Enumerable.Range(0, 4)
                .Select(i => i == Convert.ToInt32(orientation))
                .ToArray());
}
