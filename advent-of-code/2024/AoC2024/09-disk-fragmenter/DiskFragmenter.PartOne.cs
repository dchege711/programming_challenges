namespace AoC2024;

public partial class DiskFragmenter
{
    public static long PartOne(IEnumerable<int> diskMap) =>
        DefragmentFileBlocks(ExpandDiskMap(diskMap).ToArray())
            .Select((id, idx) => (long)id * idx)
            .Sum();

    private static IEnumerable<int> ExpandDiskMap(IEnumerable<int> diskMap) =>
        diskMap.SelectMany((blockSize, idx) => {
            var isFreeSpace = idx % 2 == 1;
            var identifier = isFreeSpace ? FreeBlockCanary : idx / 2;
            return Enumerable.Repeat(identifier, blockSize);
        });
    
    private static IEnumerable<int> DefragmentFileBlocks(int[] diskMap)
    {
        // Invariants:
        // - li is on an index for which we must yield a file block if possible.
        // - All blocks to the right of ri are free.
        var (li, ri) = (0, diskMap.Length - 1);
        while (IsFreeBlock(diskMap[ri]) && ri >= 0)
            ri--;

        while (li <= ri)
        {
            var liBlock = diskMap[li];
            var liIsFileBlock = !IsFreeBlock(liBlock);
            var riBlock = diskMap[ri];
            var riIsFileBlock = !IsFreeBlock(riBlock);

            if (liIsFileBlock && riIsFileBlock)
            {
                yield return liBlock;
                li++;
            }
            else if (liIsFileBlock && !riIsFileBlock)
            {
                yield return liBlock;
                li++;
                ri--;
            }
            else if (!liIsFileBlock && riIsFileBlock)
            {
                yield return riBlock;
                li++;
                ri--;
            }
            else
            {
                ri--;
            }
        }
    }

    private static bool IsFreeBlock(int block) => block == FreeBlockCanary;

    private static readonly int FreeBlockCanary = -1;
}
