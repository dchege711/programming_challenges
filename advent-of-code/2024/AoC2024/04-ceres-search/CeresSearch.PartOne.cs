namespace AoC2024;

public partial class CeresSearch
{
    static readonly private List<(int, int)> possibleMoves = [
        (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)];

    static readonly private char[] targetString = ['X', 'M', 'A', 'S'];

    public int PartOne()
    {
        int numOccurrences = 0;
        for (var r = 0; r < grid.GetLength(0); r++)
            for (var c = 0; c < grid.GetLength(1); c++)
                numOccurrences += NumOccurrences(r, c);
        return numOccurrences;
    }

    private int NumOccurrences(int r, int c)
    {
        if (grid[r, c] != targetString[0])
            return 0;
    
        int numOccurrences = 0;
        foreach (var (deltaR, deltaC) in possibleMoves)
        {
            for (var i = 1; i < targetString.Length; i++)
            {
                var nextR = r + (i * deltaR);
                var nextC = c + (i * deltaC);

                if (nextR < 0 || nextR >= grid.GetLength(0) || nextC < 0 || nextC >= grid.GetLength(1))
                    break;

                if (grid[nextR, nextC] != targetString[i])
                    break;
                    
                if (i == targetString.Length - 1)
                    numOccurrences += 1;
            }
        }

        return numOccurrences;
    }
}
