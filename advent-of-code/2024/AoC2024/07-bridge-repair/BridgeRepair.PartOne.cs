using System.Collections.Immutable;

namespace AoC2024;

public partial class BridgeRepair
{
    public static long TotalCalibrationResult(IEnumerable<CalibrationEquation> equations) =>
        TotalCalibrationResult(
            equations,
            ImmutableHashSet.Create([Operator.Add, Operator.Multiply]));

    private static long TotalCalibrationResult(
        IEnumerable<CalibrationEquation> equations, ImmutableHashSet<Operator> operators) =>
        equations
            .AsParallel()
            .Where(eq => IsValid(eq, operators))
            .Select(eq => eq.Result)
            .Sum();

    private static bool IsValid(CalibrationEquation equation, ImmutableHashSet<Operator> operators) =>
        PermutationWithReplacement(operators, [], equation.Operands.Count)
            .Any(operators => IsValid(equation, operators));

    private static IEnumerable<ImmutableList<Operator>> PermutationWithReplacement(
        ImmutableHashSet<Operator> seedOperators, ImmutableList<Operator> operators, int desiredLength)
    {
        if (operators.Count == desiredLength)
            yield return operators;
        
        if (operators.Count > desiredLength)
            yield break;
        
        foreach (var @operator in seedOperators)
            foreach (var permutation in PermutationWithReplacement(seedOperators, operators.Add(@operator), desiredLength))
                yield return permutation;
    }

    private static bool IsValid(
        CalibrationEquation equation, IEnumerable<Operator> operators)
    {
        long result = equation.Operands.First();
        foreach (var (@operator, operand) in Enumerable.Zip(operators, equation.Operands.Skip(1)))
        {
            result = @operator switch {
                Operator.Add => result + operand,
                Operator.Multiply => result * operand,
                Operator.Concatenate => Concatenate(result, operand),
                _ => throw new ArgumentException($"Unrecognized op: {@operator}")
            };
            if (result > equation.Result)
                break;
        }

        return result == equation.Result;
    }

    private enum Operator { Add, Multiply, Concatenate }
}
