namespace AoC2024;

public partial class GuardGallivant
{
    public int PartTwo() {
        int numPossibleObstaclePositions = 0;
    
        for (int r = 0; r < areaMap.GetLength(0); r++)
        {
            for (int c = 0; c < areaMap.GetLength(1); c++)
            {
                if (areaMap[r, c] != PositionState.kUnVisited)
                    continue;

                bool hasCycle = HasEndlessCycle(
                    CloneAreaMapWithObstacle(r, c),
                    startingPosition.r,
                    startingPosition.c,
                    startingPosition.dr,
                    startingPosition.dc);
                numPossibleObstaclePositions += hasCycle ? 1 : 0;
            }
        }

        return numPossibleObstaclePositions;
    }

    private bool HasEndlessCycle(PositionState[,] map, int r, int c, int dr, int dc)
    {
        var nr = r + dr;
        var nc = c + dc;
        Dictionary<(int r, int c, int dr, int dc), int> visitCounts = new();
        while (InBounds(nr, nc))
        {
            switch (map[nr, nc]) {
                case PositionState.kBlocked: {
                    (nr, nc) = (nr - dr, nc - dc); // Backtrack
                    (dr, dc) = TurnRight90Degrees(dr, dc); // Re-orient
                    break;
                }

                case PositionState.kVisited: {
                    var key = (nr, nc, dr, dc);
                    if (visitCounts.TryGetValue(key, out var visitCount))
                        return true;
                    break;
                }

                case PositionState.kUnVisited: {
                    var key = (nr, nc, dr, dc);
                    visitCounts.TryGetValue(key, out var visitCount);
                    visitCounts[key] = visitCount + 1;
                    map[nr, nc] = PositionState.kVisited;
                    break;
                }

                default:
                    throw new ArgumentException(
                        $"Invalid PositionState: {map[nr, nc]}");
            }

            (nr, nc) = (nr + dr, nc + dc);
        }

        return false;
    }

    private PositionState[,] CloneAreaMapWithObstacle(int nr, int nc)
    {
        var copy = new PositionState[areaMap.GetLength(0), areaMap.GetLength(1)];
        for (int r = 0; r < areaMap.GetLength(0); r++)
            for (int c = 0; c < areaMap.GetLength(1); c++)
                copy[r, c] = areaMap[r, c];
        
        copy[nr, nc] = PositionState.kBlocked;

        return copy;
    }
}
