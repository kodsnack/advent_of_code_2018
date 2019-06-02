import java.util.*;
import java.util.regex.Pattern;

class Day05B {

    public static void main(String[] args) {

        Scanner in = new Scanner(System.in);

        String str = in.nextLine();
        in.close();

        HashMap<String, Integer> res = new HashMap<>();

        for(char c = 'a'; c <= 'z'; c++) {
            Pattern p = Pattern.compile("[" + c + Character.toUpperCase(c) + "]");
            String local = p.matcher(str).replaceAll("");

            res.put(s, react(local));
        }

        System.out.println(res.entrySet().stream().min(Map.Entry.comparingByValue()).get().getValue());
    }

    public static int react(String str) {

        int lastround = 0;
        while (lastround != str.length()) {
            lastround = str.length();
            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < str.length(); i++) {
                if (i + 1 < str.length()) {

                    String c = str.substring(i, i + 1);
                    String c2 = str.substring(i + 1, i + 2);
                    if (
                            (c.equals(c.toLowerCase()) && c2.equals(c2.toUpperCase()) && c.toUpperCase().equals(c2)) ||
                                    (c.equals(c.toUpperCase()) && c2.equals(c2.toLowerCase()) && c.toLowerCase().equals(c2))
                    ) {
                        i++;
                    } else {
                        sb.append(c);
                    }
                } else {
                    sb.append(str.substring(i, i + 1));
                }
            }

            str = sb.toString();
        }

        return lastround;
    }
}