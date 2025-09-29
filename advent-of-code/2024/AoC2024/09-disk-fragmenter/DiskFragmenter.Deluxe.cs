namespace AoC2024;

using System.Collections.Generic;
using System.Linq;

public partial class DiskFragmenter
{
    // Represents a contiguous region (extent) on the disk
    private record struct Extent(int Start, int Length, int FileId);
    // ...existing code...
    // Returns the checksum for the defragmented disk using extents (memory efficient)
    public static long PartOneDeluxe(IEnumerable<int> diskMap)
    {
        // Direct port of DefragmentFileBlocks logic, but using runs
        // Build a list of (fileId, count) and (free, count) segments
        var runs = new List<(int fileId, int count)>();
        int fileId = 0;
        bool isFile = true;
        foreach (var size in diskMap)
        {
            runs.Add((isFile ? fileId : FreeBlockCanary, size));
            if (isFile) fileId++;
            isFile = !isFile;
        }

        int li = 0, ri = runs.Count - 1;
        int pos = 0;
        long sum = 0;
        int totalBlocks = runs.Sum(r => r.count);
        while (pos < totalBlocks)
        {
            // Find leftmost available block
            while (li < runs.Count && runs[li].count == 0) { li++; }
            // Find rightmost available block
            while (ri >= 0 && runs[ri].count == 0) { ri--; }
            if (li > ri) break;

            var liBlock = runs[li].fileId;
            var liIsFileBlock = liBlock != FreeBlockCanary;
            var riBlock = runs[ri].fileId;
            var riIsFileBlock = riBlock != FreeBlockCanary;

            if (liIsFileBlock && riIsFileBlock)
            {
                sum += (long)liBlock * pos;
                runs[li] = (liBlock, runs[li].count - 1);
                pos++;
            }
            else if (liIsFileBlock && !riIsFileBlock)
            {
                sum += (long)liBlock * pos;
                runs[li] = (liBlock, runs[li].count - 1);
                runs[ri] = (riBlock, runs[ri].count - 1);
                pos++;
                // After decrementing ri, skip contiguous free run
                if (runs[ri].fileId == FreeBlockCanary && runs[ri].count > 0)
                {
                    int skip = ContiguousLeftwardRunLength(runs, ri);
                    ri -= skip;
                }
            }
            else if (!liIsFileBlock && riIsFileBlock)
            {
                sum += (long)riBlock * pos;
                runs[ri] = (riBlock, runs[ri].count - 1);
                runs[li] = (liBlock, runs[li].count - 1);
                pos++;
            }
            else
            {
                runs[ri] = (riBlock, runs[ri].count - 1);
                pos++;
                // After decrementing ri, skip contiguous free run
                if (runs[ri].fileId == FreeBlockCanary && runs[ri].count > 0)
                {
                    int skip = ContiguousLeftwardRunLength(runs, ri);
                    ri -= skip;
                }
            }
        }
        return sum;

    // Helper: get contiguous leftward run length of free blocks in runs
    static int ContiguousLeftwardRunLength(List<(int fileId, int count)> runs, int idx)
    {
        int skip = 0;
        while (idx - skip >= 0 && runs[idx - skip].fileId == FreeBlockCanary && runs[idx - skip].count > 0)
            skip++;
        return skip;
    }
    }

    // Returns the checksum for the defragmented disk using extents (memory efficient, Part Two logic)
    public static long PartTwoDeluxe(IEnumerable<int> diskMap)
    {
        var extents = ParseExtents(diskMap);
        var defragmented = DefragmentExtentsPartTwo(extents);
        return defragmented
            .Where(e => e.FileId != FreeBlockCanary)
            .SelectMany(e => Enumerable.Range(e.Start, e.Length).Select(idx => ((long)e.FileId) * idx))
            .Sum();
    }

    // Defragments the extents by moving whole files in decreasing file ID order (Part Two logic)
    private static List<Extent> DefragmentExtentsPartTwo(List<Extent> extents)
    {
        // Copy extents to allow mutation
        var working = extents.Select(e => e).ToList();
        // Get all file extents, sorted by descending file ID
        var files = working.Where(e => e.FileId != FreeBlockCanary)
            .OrderByDescending(e => e.FileId)
            .ToList();

        foreach (var file in files)
        {
            // Find the leftmost free extent large enough and before the file
            int fileIdx = working.FindIndex(e => e.FileId == file.FileId);
            int? freeIdx = null;
            for (int i = 0; i < fileIdx; i++)
            {
                if (working[i].FileId == FreeBlockCanary && working[i].Length >= file.Length)
                {
                    freeIdx = i;
                    break;
                }
            }
            if (freeIdx is int idx)
            {
                // Move file to free extent
                var free = working[idx];
                // Replace free extent with file extent
                working[idx] = new Extent(free.Start, file.Length, file.FileId);
                // If free extent is larger, insert remaining free space after
                if (free.Length > file.Length)
                {
                    working.Insert(idx + 1, new Extent(free.Start + file.Length, free.Length - file.Length, FreeBlockCanary));
                    fileIdx++;
                }
                // Remove the original file extent
                working.RemoveAt(fileIdx);
            }
        }
        // Compact adjacent free extents
        var compacted = new List<Extent>();
        foreach (var e in working)
        {
            if (e.FileId == FreeBlockCanary &&
                compacted.Count > 0 && compacted[^1].FileId == FreeBlockCanary)
            {
                var prev = compacted[^1];
                compacted[^1] = new Extent(prev.Start, prev.Length + e.Length, FreeBlockCanary);
            }
            else
            {
                compacted.Add(e);
            }
        }
        return compacted;
    }
    // ...existing code...

    // Parses the input into a list of extents (file or free regions)
    private static List<Extent> ParseExtents(IEnumerable<int> diskMap)
    {
        var extents = new List<Extent>();
        int pos = 0;
        int fileId = 0;
        bool isFile = true;
        foreach (var size in diskMap)
        {
            extents.Add(new Extent(pos, size, isFile ? fileId++ : FreeBlockCanary));
            pos += size;
            isFile = !isFile;
        }
        return extents;
    }
}
