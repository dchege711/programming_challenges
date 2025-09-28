namespace AoC2024;

public partial class DiskFragmenter
{
    public static long PartTwo(IEnumerable<int> diskMap) =>
        DefragmentFiles(ExpandDiskMap(diskMap).ToArray())
            .Select((id, idx) => IsFreeBlock(id) ? 0L : (long)id * idx)
            .Sum();
        

    private static int[] DefragmentFiles(int[] diskMap)
    {
        var ri = diskMap.Length - 1;
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
