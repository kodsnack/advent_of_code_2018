
import java.util.*;

class Day01B {

    public static void main(String[] args) {

		ArrayList<Integer> history = new ArrayList<>();
		ArrayList<Integer> changes = new ArrayList<>();
		int freq = 0;

		Scanner in = new Scanner(System.in);
		while(in.hasNext())
			changes.add(Integer.valueOf(in.nextLine()));
		in.close();

		while(foundFrequency == false) {
			changes.forEach(change -> {
                if(history.contains(freq)) {
                    System.out.println(freq);
                    System.exit(0);
                }
                history.add(freq);
                freq += change;
			});
		}
    }
}
