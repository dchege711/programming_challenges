using System.Collections.Immutable;
using System.Text;

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
        PermutationWithReplacement(operators, [], equation.Operands.Count - 1)
            .Any(operators => IsValid(equation, operators));

    private static IEnumerable<ImmutableList<Operator>> PermutationWithReplacement(
        ImmutableHashSet<Operator> seedOperators, ImmutableList<Operator> operators, int desiredLength)
    {
        if (operators.Count == desiredLength)
        {
            Console.WriteLine($"Yielding: Base case: {string.Join(" ", operators.Select(ToString))}");
            yield return operators;
        }
        
        if (operators.Count > desiredLength)
        {
            Console.WriteLine($"Yielding: break");
            yield break;
        }
        
        foreach (var @operator in seedOperators)
        {
            foreach (var permutation in PermutationWithReplacement(seedOperators, operators.Add(@operator), desiredLength))
            {
                Console.WriteLine($"Yielding: Permutation {string.Join(" ", permutation.Select(ToString))}");
                yield return permutation;
            }
        }
    }

    private static bool IsValid(
        CalibrationEquation equation, IEnumerable<Operator> operators)
    {
        long result = equation.Operands.First();

        StringBuilder stringBuilder = new();
        stringBuilder.Append(result);

        foreach (var (@operator, operand) in Enumerable.Zip(operators, equation.Operands.Skip(1)))
        {
            result = @operator switch {
                Operator.Add => result + operand,
                Operator.Multiply => result * operand,
                Operator.Concatenate => Concatenate(result, operand),
                _ => throw new ArgumentException($"Unrecognized op: {@operator}")
            };

            stringBuilder.Append($" {ToString(@operator)} {operand}");

            if (result > equation.Result)
            {
                stringBuilder.Append($" (...can only increase from here)");
                break;
            }
        }

        stringBuilder.Append($" == {result} ?= {equation.Result} : {result == equation.Result}");
        Console.WriteLine(stringBuilder);

        return result == equation.Result;
    }

    private static string ToString(Operator @operator) => @operator switch {
        Operator.Add => "+",
        Operator.Multiply => "x",
        Operator.Concatenate => "||",
        _ => throw new ArgumentException($"Unrecognized op: {@operator}")
    };

    private enum Operator { Add, Multiply, Concatenate }
}
