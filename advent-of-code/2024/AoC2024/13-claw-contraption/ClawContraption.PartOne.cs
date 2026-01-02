namespace AoC2024;

public partial class ClawContraption
{
    public record struct DirectedEdge(Vector From, Vector To, int Cost);

    public static int PartOne(string filePath) =>
        Parse(filePath)
            .Select(GetMinimumCost)
            .Where(cost => cost != int.MaxValue)
            .Sum();

    private static int GetMinimumCost(MachineConfig config)
    {
        var pq = new PQWithReplace();
        pq.Upsert(new(0, 0), 0);
        var distTo = new Dictionary<Vector, int>([new(new(0, 0), 0)]);

        while (!pq.IsEmpty())
        {
            var v = pq.Dequeue();
            if (v == config.Prize)
                break;

            foreach (var edge in v.GetEdges(config))
            {
                var prevCost = GetCost(distTo, edge.To);
                var currCost = GetCost(distTo, v) + edge.Cost;
                if (prevCost > currCost)
                {
                    distTo[edge.To] = currCost;
                    pq.Upsert(edge.To, currCost);
                }
            }
        }

        return GetCost(distTo, config.Prize);
    }

    private static int GetCost(Dictionary<Vector, int> distTo, Vector v)
    {
        if (distTo.TryGetValue(v, out var cost))
            return cost;
        
        return int.MaxValue;
    }
}

public class PQWithReplace
{
    private record struct VectorAndCost(ClawContraption.Vector Vector, int Cost);
    private SortedSet<VectorAndCost> _set = new(new VectorAndCostComp());

    public bool IsEmpty() => _set.Count == 0;

    public void Upsert(ClawContraption.Vector v, int cost)
    {
        var matches = _set.Where(vc => vc.Vector == v).ToArray();
        if (matches.Length > 1)
            throw new Exception($"Multiple {v} found in PQ.");
        
        if (matches.Length == 1)
            _set.Remove(matches[0]);
        
        _set.Add(new(v, cost));
    }
    
    public ClawContraption.Vector Dequeue() {
        var vc = _set.First();
        _set.Remove(vc);
        return vc.Vector;
    }

    private class VectorAndCostComp : IComparer<VectorAndCost>
    {
        public int Compare(VectorAndCost lhs, VectorAndCost rhs)
        {
            // First, compare by cost in a PQ fashion.
            var comp = lhs.Cost.CompareTo(rhs.Cost);
            if (comp != 0)
                return comp;
            
            // Then compare by vector coordinates.
            comp = lhs.Vector.X.CompareTo(rhs.Vector.X);
            if (comp != 0)
                return comp;
            
            return lhs.Vector.Y.CompareTo(rhs.Vector.Y);
        }
    }
}