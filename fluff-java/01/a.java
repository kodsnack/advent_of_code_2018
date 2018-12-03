
import java.util.*;

class Day01A {

    public static void main(String[] args) {
		int freq = 0;

		Scanner in = new Scanner(System.in);
		while(in.hasNext())
			freq += Integer.parseInt(in.nextLine());

		in.close();
		System.out.println(freq);
    }
}
