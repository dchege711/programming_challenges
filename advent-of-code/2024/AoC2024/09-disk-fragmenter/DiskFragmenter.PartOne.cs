namespace AoC2024;

public partial class DiskFragmenter
{
    // public static long PartOne(IEnumerable<int> diskMap) =>
    //     DefragmentedFileBlocks(diskMap.ToArray())
    //         .Select((id, idx) => id * idx)
    //         .Sum();

    // 2333133121414131402
    // 0.1.2.3.4.5.6.7.8.9
    // 00...111...2...333.44.5555.6666.777.888899
    // 0099811188827773336446555566..............

    public static long PartOne(IEnumerable<int> diskMap) => PartOneBruteForce(diskMap);

    private static long PartOneBruteForce(IEnumerable<int> diskMap)
    {
        var expandedDiskMap = ExpandDiskMap(diskMap).ToArray();
        var defragmentedDiskMap = DefragmentedFileBlocksFromExpansion(expandedDiskMap).ToArray();
        return defragmentedDiskMap.Select((id, idx) => (long)id * idx).Sum();
    }

    private static long PartOneDeluxe(IEnumerable<int> diskMap)
    {
        var blocks = DefragmentedFileBlocks(diskMap.ToArray()).ToArray();
        return blocks.Select((id, idx) => id * idx).Sum();
    }

    private static IEnumerable<int> DefragmentedFileBlocks(int[] diskMap)
    {
        var (li, ri) = (0, diskMap.Length - 1);
        var (numFreeBlocksAvailable, numFileBlocksMovable) = (-1, -1);
        while (li < diskMap.Length)
        {
            var liOnFile = li % 2 == 0;
            var liBlockSize = diskMap[li];

            // Yield li is on a file as its blocks prevents any defragmentation.
            if (liOnFile)
            {
                var fileIndex = li / 2;
                foreach (var block in Enumerable.Repeat(fileIndex, liBlockSize))
                    yield return block;

                li++;
                continue;
            }

            var riOnFile = ri % 2 == 0;
            var riBlockSize = diskMap[ri];

            numFreeBlocksAvailable = numFreeBlocksAvailable == -1
                ? liBlockSize
                : numFreeBlocksAvailable;
            
            // If ri is on a free space, move to the next candidate for
            // defragmentation.
            if (!riOnFile)
            {
                ri--;
                continue;
            }

            numFileBlocksMovable = numFileBlocksMovable == -1
                ? riBlockSize
                : numFileBlocksMovable;

            var numFileBlocksToMove = numFreeBlocksAvailable > numFileBlocksMovable
                ? numFileBlocksMovable
                : numFreeBlocksAvailable;
            
            // Defragment any file blocks on the right that can fit.
            if (numFileBlocksToMove > 0)
            {
                var fileIndex = ri / 2;
                foreach (var block in Enumerable.Repeat(fileIndex, numFileBlocksToMove))
                    yield return block;
                
                numFreeBlocksAvailable -= numFileBlocksToMove;
                numFileBlocksMovable -= numFileBlocksToMove;
            }

            // Advance at 1 of the pointers.
            if (numFileBlocksToMove >= riBlockSize)
            {
                numFileBlocksMovable = -1;
                ri--;
            }
            else
            {
                numFreeBlocksAvailable = -1;
                li++;
            }
        }

        yield break;
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
