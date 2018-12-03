import java.util.*;

class Day02B {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        ArrayList<ArrayList<String>> ids = new ArrayList<>();

        while(in.hasNext()) {
            ArrayList<String> candidate = new ArrayList<>();
            candidate.addAll(Arrays.asList(in.nextLine().split("")));

            ids.forEach(matcher -> {
                ArrayList<String> incorrectCandidateLetters = new ArrayList<>();
                for(int i = 0; i < matcher.size(); i++) {
                    if(!matcher.get(i).equals(candidate.get(i)))
                        incorrectCandidateLetters.add(candidate.get(i));
                }

                if(incorrectCandidateLetters.size() == 1)
                    System.out.println(String.join("", candidate).replaceFirst(incorrectCandidateLetters.get(0), ""));
            });

            ids.add(candidate);
        }
        in.close();
    }
}