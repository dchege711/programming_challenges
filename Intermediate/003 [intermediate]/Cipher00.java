/**
 * Takes a piece of text and encrypts it with an alphabetical substitution cipher.
 *
 * Note: Incomplete...
 */

import java.lang.StringBuilder;

public class Cipher00 {

  private char[] cipherText;
  private int[] cipherKey;
  private int alphabetLength = 128;

  /**
   * Instantiate this class.
   */
  public Cipher00() {
    this.cipherKey = new int[alphabetLength];
    this.cipherText = new char[alphabetLength];

    for (int i = 0; i < alphabetLength; i++) {
      cipherText[i] = (char)i;
    }

    // Shuffle the array...
    // System.out.println(cipherText);
    for (int i = 0; i < alphabetLength; i++) {
      int randomIndex = (int)(Math.random() * alphabetLength);
      char temp = cipherText[randomIndex];
      cipherText[randomIndex] = cipherText[i];
      cipherText[i] = temp;
    }
    // System.out.println(cipherText);

    // Fill in the key
    for (int i = 0; i < alphabetLength; i++) {
      cipherKey[(int)(cipherText[i])] = i;
    }

    for (int i = 0; i < alphabetLength; i++) {
      System.out.print((int)(cipherKey[i]) + " ");
    }
    System.out.println();

    for (int i = 0; i < alphabetLength; i++) {
      System.out.print((int)(cipherText[i]) + " ");
    }
    System.out.println();

  }

  /**
   * Encrypt a string using the cipher text.
   */
  public String encrypt(String s) {
    StringBuilder encrypted = new StringBuilder();
    for (int i = 0; i < s.length(); i++) {
      encrypted.append(cipherText[(int)(s.charAt(i))]);
    }
    return encrypted.toString();
  }

  /**
   * Decrypt an encrypted string using the cipher text.
   */
  public String decrypt(String s) {
    StringBuilder decrypted = new StringBuilder();
    for (int i = 0; i < s.length(); i++) {
      int index = cipherKey[(int)(s.charAt(i))];
      decrypted.append(cipherText[index]);
    }
    return decrypted.toString();
  }

  /**
   * Unit test.
   */
  public static void main(String[] args) {
    Cipher00 cipher = new Cipher00();
    String test = "This is a test.";
    String encrypted = cipher.encrypt(test);
    String decrypted = cipher.decrypt(encrypted);
    // System.out.print(test + " --> " + encrypted + " --> " + decrypted);
  }

}
