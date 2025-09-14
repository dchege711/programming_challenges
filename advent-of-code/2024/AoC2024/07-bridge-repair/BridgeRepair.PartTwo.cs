using System.Collections.Immutable;

namespace AoC2024;

public partial class BridgeRepair
{
    public static long TotalCalibrationResultWithConcat(IEnumerable<CalibrationEquation> equations) =>
        TotalCalibrationResult(
            equations,
            ImmutableHashSet.Create([Operator.Add, Operator.Multiply, Operator.Concatenate]));
    
    private static long Concatenate(long a, long b)
    {
        var powerOf10 = Math.Pow(10, Math.Floor(Math.Log10(b)) + 1);
        return (a * (long)powerOf10) + b;
    }
}
