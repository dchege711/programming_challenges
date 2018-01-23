import java.util.LinkedList;
import java.lang.StringBuilder;

public class LFSR347 {
    
    private LinkedList<Integer> lfsr;
    private int[] tapPositions;
    
    public LFSR347(int[] initialValues, int[] tapPositions) {
        this.lfsr = new LinkedList<Integer>();
        this.tapPositions = tapPositions;
        for (int n: initialValues) {
            this.lfsr.add(n);
        }
    }
    
    public String toString() {
        Object[] array = this.lfsr.toArray();
        StringBuilder sb = new StringBuilder();
        for (Object n: array) sb.append(n);
        return sb.toString();
    }
    
    public String step(String operation) {
        int result = this.lfsr.get(this.tapPositions[0]);
        for (int i = 1; i < this.tapPositions.length; i++) {
            result = xor(result, this.lfsr.get(this.tapPositions[i]));
            if (operation == "XNOR") result = !result
            elif (operation != "XOR") throw new IllegalArgumentException();
        }
        this.lfsr.removeLast();
        this.lfsr.addFirst(result);
        return toString();
    }
    
    public int xor(int a, int b) {
        return a ^ b;
    }
    
    public static void main(String[] args) {
        int[] initialValues = {0, 0, 1};
        int[] tapPositions = {0, 2};
        LFSR347 testLFSR = new LFSR347(initialValues, tapPositions);
        System.out.println(testLFSR.toString());
        for (int i = 1; i < 8; i++) {
            System.out.println(i + " " + testLFSR.step("XdOR"));
        }
    }
    
    
} 
