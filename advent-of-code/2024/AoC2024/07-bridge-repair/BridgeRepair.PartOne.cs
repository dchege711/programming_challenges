using System.Collections.Immutable;

namespace AoC2024;

public partial class BridgeRepair
{
    public static long TotalCalibrationResult(IEnumerable<CalibrationEquation> equations) =>
        equations
            .Where(IsValid)
            .Select(eq => eq.Result)
            .Sum();

    private static bool IsValid(CalibrationEquation equation) =>
        PermutationWithReplacement([], equation.Operands.Count)
            .Any(operators => IsValid(equation, operators));

    private static IEnumerable<ImmutableList<Operator>> PermutationWithReplacement(
        ImmutableList<Operator> operators, int desiredLength)
    {
        if (operators.Count == desiredLength)
            yield return operators;
        
        if (operators.Count > desiredLength)
            yield break;
        
        foreach (var permutation in PermutationWithReplacement(operators.Add(Operator.Add), desiredLength))
            yield return permutation;
        
        foreach (var permutation in PermutationWithReplacement(operators.Add(Operator.Multiply), desiredLength))
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
                _ => throw new ArgumentException($"Unrecognized op: {@operator}")
            };
            if (result > equation.Result)
                break;
        }

        return result == equation.Result;
    }

    private static ImmutableList<Operator> Operators = ImmutableList.Create(
        [Operator.Add, Operator.Multiply]);

    private enum Operator { Add, Multiply }
}
