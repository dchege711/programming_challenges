import java.util.LinkedList;
import java.util.BitSet;
import java.lang.StringBuilder;

public class LFSR347 {
    
    private LinkedList<BitSet> lfsr;
    private int[] tapPositions;
    
    public LFSR347(int[] initialValues, int[] tapPositions) {
        this.lfsr = new LinkedList<BitSet>();
        this.tapPositions = tapPositions;
        for (int n: initialValues) {
            this.lfsr.add(getBit(n));
        }
        System.out.println(toString() + " first = " + this.lfsr.getFirst() + " last = " + this.lfsr.getLast() + "\n");
    }
    
    public BitSet getBit(int a) {
        BitSet b = new BitSet(1);
        if (a == 1) b.set(1);
        else if (a != 0) throw new IllegalArgumentException();
        return b;
    }
    
    public String toString() {
        Object[] array = this.lfsr.toArray();
        StringBuilder sb = new StringBuilder();
        for (Object n: array) sb.append(n);
        return sb.toString();
    }
    
    public String step(String operation) {
        
        BitSet result = this.lfsr.get(this.tapPositions[0]);
        for (int i = 1; i < this.tapPositions.length; i++) {
            // System.out.print(" " + result);
            result.xor(this.lfsr.get(this.tapPositions[i]));
            if (operation == "XNOR") result.flip(0);
            else if (operation != "XOR") throw new IllegalArgumentException();
            // System.out.print(" " + operation + " " + this.lfsr.get(this.tapPositions[i]));
            // System.out.print(" = " + result);
        }
        
        BitSet r = this.lfsr.removeLast();
        System.out.print(" : dropped " + r + " ... " + toString());
        this.lfsr.addFirst(result);
        System.out.println(" added " + result + " ... " + toString());
        return toString();
    }
    
    public int xor(int a, int b) {
        return a ^ b;
    }
    
    public static void main(String[] args) {
        int[] initialValues = {0, 0, 1};
        int[] tapPositions = {0, 2};
        LFSR347 testLFSR = new LFSR347(initialValues, tapPositions);
        System.out.println("0 " + testLFSR.toString() + "\n");
        for (int i = 1; i < 8; i++) {
            System.out.println("\n" + i + " " + testLFSR.step("XOR") + "\n");
        }
    }
    
    
} 
