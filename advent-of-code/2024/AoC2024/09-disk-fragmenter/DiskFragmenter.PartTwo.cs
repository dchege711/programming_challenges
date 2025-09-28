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

        var ri = diskMap.Length - 1;
        while (IsFreeBlock(diskMap[ri]) && ri >= 0)
            ri--;

        while (ri > 0)
        {
            var riBlock = diskMap[ri];
            var riBlockSize = ContiguousSizeFromIndex(diskMap, ri, -1);

            if (IsFreeBlock(riBlock))
            {
                ri -= riBlockSize;
                continue;
            }

            var maybeNewLocation = GetFirstAvailableFreeSpace(diskMap, ri - riBlockSize + 1, riBlockSize);
            if (maybeNewLocation is int newLocation)
            {
                // Defragment ri's block by moving to left-most available block.
                for (int i = 0; i < riBlockSize; i++)
                {
                    diskMap[newLocation + i] = riBlock;
                    diskMap[ri - i] = FreeBlockCanary;
                }
            }
            
            ri -= riBlockSize;
        }

        return diskMap;
    }

    private static int ContiguousSizeFromIndex(int[] diskMap, int idx, int delta)
    {
        var size = 0;
        var target = diskMap[idx];
        for (int i = idx; i < diskMap.Length && i >= 0 && diskMap[i] == target; i += delta)
            size++;
        return size;
    }

    private static int? GetFirstAvailableFreeSpace(int[] diskMap, int blockStartIdx, int blockSize)
    {
        int i = 0;
        while (i + blockSize <= blockStartIdx)
        {
            var currentBlockSize = ContiguousSizeFromIndex(diskMap, i, 1);
            if (blockSize <= currentBlockSize && IsFreeBlock(diskMap[i]))
                return i;
            
            i += currentBlockSize;
        }

        return null;
    }
}
