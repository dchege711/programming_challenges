namespace AoC2024.RestroomRedoubtDataTypes;

public record struct Vector(int X, int Y);

public record struct Robot(Vector Position, Vector Velocity);

public enum Quadrant { kTopLeft, kTopRight, kBottomRight, kBottomLeft };

public static class RestroomRedoubtExtensions
{
    public static Vector WrapAround(this Vector self, int areaWidth, int areaHeight) =>
        new(self.X % areaWidth, self.Y % areaHeight);

    public static Vector MoveBy(this Vector self, Vector velocity, int duration) =>
        new(self.X + velocity.X * duration, self.Y + velocity.Y * duration);
}