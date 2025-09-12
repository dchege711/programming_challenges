namespace AoC2024;

public partial class GuardGallivant
{
    public int PartTwo() {
        int numPossibleObstaclePositions = 0;
    
        for (int r = 0; r < AreaMap.RowCount; r++)
        {
            for (int c = 0; c < AreaMap.ColCount; c++)
            {
                Coordinate coordinate = new(r, c);
                if (StartingPosition.Coordinate == coordinate
                    || AreaMap.Obstacles.Contains(coordinate))
                    continue;
                
                AreaMap.Obstacles.Add(coordinate);
                numPossibleObstaclePositions += SimulateGuardMovement().IsTrapped ? 1 : 0;
                AreaMap.Obstacles.Remove(coordinate);
            }
        }

        return numPossibleObstaclePositions;
    }
}
