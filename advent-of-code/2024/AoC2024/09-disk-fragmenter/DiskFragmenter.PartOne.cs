namespace AoC2024;

public partial class DiskFragmenter
{
    public static long PartOne(IEnumerable<int> diskMap)
    {
        var expandedDiskMap = ExpandDiskMap(diskMap).ToArray();
        var defragmentedDiskMap = DefragmentedFileBlocksFromExpansion(expandedDiskMap).ToArray();
        return defragmentedDiskMap.Select((id, idx) => (long)id * idx).Sum();
    }

    private static IEnumerable<int> ExpandDiskMap(IEnumerable<int> diskMap) =>
        diskMap.SelectMany((blockSize, idx) => {
            var isFreeSpace = idx % 2 == 1;
            var identifier = isFreeSpace ? FreeBlockCanary : idx / 2;
            return Enumerable.Repeat(identifier, blockSize);
        });
    
    private static IEnumerable<int> DefragmentedFileBlocksFromExpansion(int[] diskMap)
    {
        var (li, ri) = (0, diskMap.Length - 1);
        var expectedSize = diskMap.Count(id => id != FreeBlockCanary);
        var yieldedSize = 0;

        while (li < diskMap.Length)
        {
            if (yieldedSize == expectedSize)
                break;

            var blockId = diskMap[li];

            // Case 1: An actual file block
            var isFileBlock = blockId != FreeBlockCanary;
            if (isFileBlock)
            {
                yield return blockId;
                yieldedSize++;
                li++;
                continue;
            }
            
            // Case 2: An empty space: Try moving the right most block
            blockId = diskMap[ri];
            isFileBlock = blockId != FreeBlockCanary;

            // Case 2a: Right block is a file block
            if (isFileBlock)
            {
                diskMap[li] = blockId;
                diskMap[ri] = FreeBlockCanary;
                ri--;
                continue;
            }

            // Case 2b: Right block is free and left pointer has caught up.
            if (li >= ri)
                yield break;

            // Case 2c: Right block is free, but left pointer still has some
            // indices to potentially yield from.
            ri--;
        }

        yield break;
    }

    private static readonly int FreeBlockCanary = -1;
}
