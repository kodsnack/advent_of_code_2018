import java.awt.*;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Day03A {

    public static void main(String[] args) {
        Pattern pattern = Pattern.compile("^\\#(\\d+)\\ \\@\\ (\\d+)\\,(\\d+)\\:\\ (\\d+)x(\\d+)$");
        HashMap<String, Integer> cloth = new HashMap<>();

        Scanner in = new Scanner(System.in);
        while(in.hasNext()) {
            String str = in.nextLine();
            Matcher matcher = pattern.matcher(str);

            while(matcher.find()) {
                int id = Integer.valueOf(matcher.group(1));
                int startX = Integer.valueOf(matcher.group(2));
                int startY = Integer.valueOf(matcher.group(3));
                int width = Integer.valueOf(matcher.group(4));
                int height = Integer.valueOf(matcher.group(5));

                for(int x = startX; x < startX + width; x++) {
                    for(int y = startY; y < startY + height; y++) {
                        String key = String.valueOf(x) + "|" + String.valueOf(y);
                        if(cloth.containsKey(key))
                            cloth.put(key, cloth.get(key) + 1);
                        else
                            cloth.put(key, 1);
                    }
                }
            }
        }
        in.close();

        int overlaps = 0;
        for(Map.Entry<String, Integer> entry : cloth.entrySet()) {
            if(entry.getValue() > 1)
                overlaps++;
        }

        System.out.println(overlaps);
    }
}