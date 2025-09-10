namespace AoC2024;

public partial class GuardGallivant
{
    public readonly PositionState[,] areaMap;
    public readonly (int r, int c, int dr, int dc) startingPosition;

    public enum PositionState { kBlocked, kVisited, kUnVisited }

    public GuardGallivant(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        List<char[]> rows = [];

        string? line;
        while((line = inputReader.ReadLine()) is not null)
            rows.Add(line.ToCharArray());
        
        int rowCount = rows.Count;
        int colCount = rows.First().Length;

        areaMap = new PositionState[rowCount, colCount];
        for (int r = 0; r < rowCount; r++)
        {
            foreach (var (c, val) in rows[r].Index())
            {
                var state = GetPositionState(val);
                areaMap[r, c] = state;

                if (state == PositionState.kVisited)
                {
                    var (dr, dc) = GetOrientation(val);
                    startingPosition = (r, c, dr, dc);
                }
            }
        }

    }

    private static (int dr, int dc) GetOrientation(char val) => val switch {
        '^' => (-1, 0),
        '<' => (0, -1),
        '>' => (0, 1),
        'v' => (1, 0),
        _ => throw new ArgumentException($"Unrecognized input: {val}")
    };

    private static PositionState GetPositionState(char c) => c switch {
        '.' => PositionState.kUnVisited,
        '#' => PositionState.kBlocked,
        '^' => PositionState.kVisited,
        '<' => PositionState.kVisited,
        '>' => PositionState.kVisited,
        'v' => PositionState.kVisited,
        _ => throw new ArgumentException($"Unrecognized input: {c}")
    };
}
