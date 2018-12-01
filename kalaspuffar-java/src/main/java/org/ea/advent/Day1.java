package org.ea.advent;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

public class Day1 {
    public static void main(String[] args) {
        try {
            List<Integer> allResults = new ArrayList<Integer>();

            int total = 0;
            int numInterations = 0;
            while(true) {
                BufferedReader br = new BufferedReader(
                        new FileReader("day1-input.txt")
                );
                String line;
                while ((line = br.readLine()) != null) {
                    total += Integer.parseInt(line);
                    if(allResults.contains(total)) {
                        System.out.println(total);
                        System.out.println(numInterations);
                        System.exit(0);
                    }
                    allResults.add(total);
                    numInterations++;
                }
                br.close();
            }

        } catch(Exception e) {
            e.printStackTrace();
        }
    }
}
