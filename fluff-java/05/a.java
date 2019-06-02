import java.util.*;

class Day05A {

    public static void main(String[] args) {

        Scanner in = new Scanner(System.in);
        String str = in.nextLine();
        in.close();

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

        System.out.println(lastround);
    }
}