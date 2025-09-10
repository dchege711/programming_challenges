namespace AoC2024;

public partial class GuardGallivant
{
    public int PartOne()
    {
        SimulateGuardMoves(
            startingPosition.r,
            startingPosition.c,
            startingPosition.dr,
            startingPosition.dc);
        
        return areaMap.Cast<PositionState>()
            .Count(state => state == PositionState.kVisited);;
    }

    private void SimulateGuardMoves(int r, int c, int dr, int dc)
    {
        var nr = r + dr;
        var nc = c + dc;
        while (InBounds(nr, nc))
        {
            switch (areaMap[nr, nc]) {
                case PositionState.kBlocked: {
                    (nr, nc) = (nr - dr, nc - dc); // Backtrack
                    (dr, dc) = TurnRight90Degrees(dr, dc); // Re-orient
                    break;
                }

                case PositionState.kVisited: {
                    break;
                }

                case PositionState.kUnVisited: {
                    areaMap[nr, nc] = PositionState.kVisited;
                    break;
                }

                default:
                    throw new ArgumentException(
                        $"Invalid PositionState: {areaMap[nr, nc]}");
            }

            (nr, nc) = (nr + dr, nc + dc);
        }
    }

    private static (int dr, int dc) TurnRight90Degrees(int dr, int dc) => (dr, dc) switch {
        { dr: > 0, dc: 0 } => (0, -dr),     // Down -> Left
        { dr: 0, dc: < 0 } => (dc, 0),      // Left -> Up
        { dr: < 0, dc: 0 } => (0, -dr),     // Up -> Right
        { dr: 0, dc: > 0 } => (dc, 0),      // Right -> Down
        _ => throw new ArgumentException("Invalid direction vector")
    };

    private bool InBounds(int r, int c) =>
        r >= 0 && r < areaMap.GetLength(0) && c >= 0 && c < areaMap.GetLength(1);
}
