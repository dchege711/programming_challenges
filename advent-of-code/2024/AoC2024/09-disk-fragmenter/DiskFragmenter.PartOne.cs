namespace AoC2024;

public partial class DiskFragmenter
{
    // public static long PartOne(IEnumerable<int> diskMap) =>
    //     DefragmentedFileBlocks(diskMap.ToArray())
    //         .Select((id, idx) => id * idx)
    //         .Sum();

    // 303
    // 000111
    // 012345
    // 

    public static long PartOne(IEnumerable<int> diskMap)
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
}
