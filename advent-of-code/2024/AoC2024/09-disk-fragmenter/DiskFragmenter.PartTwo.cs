namespace AoC2024;

public partial class DiskFragmenter
{
    public static long PartTwo(IEnumerable<int> diskMap)
    {
        var defragmentedFiles = DefragmentFiles(ExpandDiskMap(diskMap).ToArray()).ToArray();
        return defragmentedFiles
            .Select((id, idx) => IsFreeBlock(id) ? 0L : (long)id * idx)
            .Sum();
    }
        

    private static IEnumerable<int> DefragmentFiles(int[] diskMap)
    {
        /**
        00...111...2...333.44.5555.6666.777.888899
        0099.111...2...333.44.5555.6666.777.8888..
        0099.1117772...333.44.5555.6666.....8888..
        0099.111777244.333....5555.6666.....8888..
        00992111777.44.333....5555.6666.....8888..
        */
        var (li, ri) = (0, diskMap.Length - 1);
        while (IsFreeBlock(diskMap[ri]) && ri >= 0)
            ri--;

        // Invariant: li is always on an index whose block is yet to be known.
        while (li < diskMap.Length)
        {
            var liBlock = diskMap[li];
            var liIsFileBlock = !IsFreeBlock(liBlock);
            var liBlockSize = ContiguousSizeFromIndex(li, 1, diskMap);

            // If li is on a file block, yield; nothing else can be here.
            if (liIsFileBlock)
            {
                for (int i = 0; i < liBlockSize; i++)
                    yield return liBlock;
                li += liBlockSize;
                continue;
            }

            var liUsedFreeSpace = false;
            while (ri > li)
            {
                var riBlock = diskMap[ri];
                var riIsFileBlock = !IsFreeBlock(riBlock);
                var riBlockSize = ContiguousSizeFromIndex(ri, -1, diskMap);

                // Found the right-most candidate that fits; defragment. 
                if (riIsFileBlock && riBlockSize <= liBlockSize)
                {
                    for (int i = 0; i < riBlockSize; i++)
                    {
                        diskMap[ri - i] = FreeBlockCanary;
                        yield return riBlock;
                    }
                    
                    li += riBlockSize;
                    liUsedFreeSpace = true;
                    break;
                }

                ri -= riBlockSize;
            }

            if (!liUsedFreeSpace)
            {
                for (int i = 0; i < liBlockSize; i++)
                    yield return FreeBlockCanary;
                li += liBlockSize;
            }
        }
    }

    private static int ContiguousSizeFromIndex(int idx, int delta, int[] diskMap)
    {
        var size = 0;
        var target = diskMap[idx];
        for (int i = idx; i < diskMap.Length && i >= 0 && diskMap[i] == target; i += delta)
            size++;
        return size;
    }
}
