package org.ea.advent;

import javax.swing.text.html.HTMLDocument;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class Day2b {
    public static void main(String[] args) {
        try {
            BufferedReader br = new BufferedReader(
                    new FileReader("day2-input.txt")
            );

            List<String> allTheStrings = new ArrayList<String>();

            String line;
            while ((line = br.readLine()) != null) {
                allTheStrings.add(line);
            }

            Iterator<String> it1 = allTheStrings.iterator();
            Iterator<String> it2;
            String correctBoxId = "";

            firstLineWhile:
            while(it1.hasNext()) {
                String line1 = it1.next();
                it2 = allTheStrings.iterator();

                secondLineWhile:
                while(it2.hasNext()) {
                    boolean foundDifference = false;
                    String line2 = it2.next();
                    for (int i = 0; i < line1.length(); i++) {
                        char c1 = line1.charAt(i);
                        char c2 = line2.charAt(i);

                        if(c1 != c2) {
                            if(foundDifference) {
                                continue secondLineWhile;
                            }
                            foundDifference = true;
                        }
                    }

                    if(foundDifference) {
                        for (int i = 0; i < line1.length(); i++) {
                            char c1 = line1.charAt(i);
                            char c2 = line2.charAt(i);

                            if(c1 == c2) {
                                correctBoxId += c1;
                            }
                        }
                        System.out.println(correctBoxId);
                        System.exit(0);
                    }
                }
            }


        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
