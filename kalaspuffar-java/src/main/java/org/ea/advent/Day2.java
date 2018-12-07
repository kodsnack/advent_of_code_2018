package org.ea.advent;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Arrays;

public class Day2 {
    public static void main(String[] args) {
        String ALPHABET = "abcdefghijklmnopqrstuvwyxz";
        int[] chars;

        int triplets = 0;
        int doubles = 0;
        try {
            BufferedReader br = new BufferedReader(
                    new FileReader("day2-input.txt")
            );
            String line;
            while ((line = br.readLine()) != null) {
                chars = new int[26];
                Arrays.fill(chars, 0);

                for(int i=0; i < line.length(); i++) {
                    char c = line.charAt(i);
                    chars[ALPHABET.indexOf(c)]++;
                }

                boolean doubleInst = false;
                boolean tripletInst = false;
                for(int numOfInstances : chars) {
                    if(numOfInstances == 2) {
                        doubleInst = true;
                    }
                    if(numOfInstances == 3) {
                        tripletInst = true;
                    }
                }

                doubles += doubleInst ? 1 : 0;
                triplets += tripletInst ? 1 : 0;
            }

            System.out.println(doubles * triplets);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
