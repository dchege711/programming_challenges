namespace AoC2024;

public partial class GuardGallivant
{
    public readonly PositionState[,] areaMap;
    public readonly (int, int) startingPosition;

    public enum PositionState { kBlocked, kVisited, kUnVisited }

    public GuardGallivant(string filePath)
    {
        using StreamReader inputReader = new(filePath);
        List<IEnumerable<PositionState>> rows = [];

        string? line;
        while((line = inputReader.ReadLine()) is not null)
            rows.Add(line.ToCharArray().Select(ToState));
        
        int rowCount = rows.Count;
        int colCount = rows.First().Count();

        areaMap = new PositionState[rowCount, colCount];
        for (int r = 0; r < rowCount; r++)
        {
            foreach (var (c, state) in rows[r].Index())
            {
                areaMap[r, c] = state;
                if (state == PositionState.kVisited)
                    startingPosition = (r, c);
            }
        }

    }

    private static PositionState ToState(char c) => c switch {
        '.' => PositionState.kUnVisited,
        '#' => PositionState.kBlocked,
        '^' => PositionState.kVisited,
        _ => throw new ArgumentException($"Unrecognized input: {c}")
    };
}
