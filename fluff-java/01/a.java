
import java.util.*;

class Day01A {

    public static void main(String[] args) {

	int freq = 0;
	
	Scanner in = new Scanner(System.in);
	while(in.hasNext()) {
	    int change = Integer.valueOf(in.nextLine());
	    freq += change;
	}
	in.close();
	System.out.println(freq);
    }
}
