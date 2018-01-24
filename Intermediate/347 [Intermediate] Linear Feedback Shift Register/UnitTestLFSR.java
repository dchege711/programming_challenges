import java.io.BufferedReader;
import java.io.FileReader;

public class UnitTestLFSR {
    
    public static void main(String[] args) {
        
        String[] testFiles = {
            "347_test_1", "347_test_2",
            "347_test_3", "347_test_4" 
        };
        
        for (String fileName: testFiles) {
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            
            String[] tapPositionsStrings = bufferedReader.readLine().split(",");
            int[] tapPositions = new int[tapPositionsStrings.length];
            for (int i = 0; i < tapPositions.length; i++) {
                tapPositions[i] = Integer.parseInt(tapPositionsStrings[i]);
            }
            
            String[] details = bufferedReader.readLine().split("\\s");
            
            int[] initialValues = new int[details[1].length()];
            for (int i = 0; i < initialValues.length; i++) {
                initialValues[i] = Character.getNumericValue(details[1].charAt(i));
            }
            
            LFSR347Hack testLFSR = new LFSR347Hack(initialValues, tapPositions);
            
            String firstline = bufferedReader.readLine().split(" ")[1];
            assert testLFSR.toString() == firstline: "Error in copying initial values";
            
            String operation = details[0];
            int numberOfIterations = Integer.parseInt(details[2]);
            for (int i = 0; i < numberOfIterations; i++) {
                assert testLFSR.step(operation) == bufferedReader.readLine().split(" ")[1];
            }
            System.out.println("Passed tests for " + fileName);
        }
    }
}
