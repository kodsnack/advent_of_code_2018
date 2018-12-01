
import java.util.*;

class Day01B {

    public static void main(String[] args) {

	ArrayList<Integer> history = new ArrayList<>();
	ArrayList<Integer> changes = new ArrayList<>();
	int freq = 0;
	
	Scanner in = new Scanner(System.in);
	while(in.hasNext()) {
	    int change = Integer.valueOf(in.nextLine());
	    changes.add(change);
	}
	in.close();

	boolean foundFrequency = false;
	while(foundFrequency == false) {
	    for(int change : changes) {
		if(history.contains(freq)) {
		    foundFrequency = true;
		    break;
		}

		history.add(freq);
		freq += change;
	    }
	}
	System.out.println(freq);
    }
}
