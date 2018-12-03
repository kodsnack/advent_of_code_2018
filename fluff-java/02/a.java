import java.util.*;
import java.util.function.Predicate;

class Day02A {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int twotimers = 0;
        int threetimers = 0;

        Predicate<Integer> predicateTwo = v -> v == 2;
        Predicate<Integer> predicateThree = v -> v == 3;

        while(in.hasNext()) {
            HashMap<String, Integer> occurrances = new HashMap<>();
            Arrays.asList(in.nextLine().split("")).forEach(c -> {
                if(occurrances.containsKey(c))
                    occurrances.replace(c, occurrances.get(c) + 1);
                else
                    occurrances.put(c, 1);
            });

            if(occurrances.values().stream().anyMatch(predicateTwo))
                twotimers++;

            if(occurrances.values().stream().anyMatch(predicateThree))
                threetimers++;
        }
        in.close();

        System.out.println(twotimers * threetimers);
    }
}
