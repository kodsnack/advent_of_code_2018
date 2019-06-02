import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class Day07A {

    public static void main(String[] args) {

        HashMap<String, Step> steps = new HashMap<>();
        StringBuilder sb = new StringBuilder();
        Pattern p = Pattern.compile("Step\\ (\\w)\\ must\\ be\\ finished\\ before\\ step\\ (\\w)\\ can\\ begin.");
        Scanner in = new Scanner(System.in);
        while(in.hasNext()) {
            String str = in.nextLine();
            Matcher matcher = p.matcher(str);
            if(((Matcher) matcher).matches()) {
                String before = matcher.group(1);
                String after = matcher.group(2);

                if(!steps.containsKey(before))
                    steps.put(before, new Step(before, new ArrayList<Rule>()));

                if(!steps.containsKey(after))
                    steps.put(after, new Step(after, new ArrayList<Rule>()));

                steps.get(after).getRules().add(new Rule(before, after));
            }
        }

        boolean stepstogo = true;

        while(stepstogo) {
            Step nextStep = findSteps(steps);
            if(nextStep != null) {
                sb.append(nextStep.getStep());
                steps.get(nextStep.getStep()).setUsed(true);
                steps = removeRulesDependingOnStep(steps, nextStep);
            }
            else {
                stepstogo = false;
            }
        }

        System.out.println(sb.toString());

    }

    public static Step findSteps(HashMap<String, Step> steps) {

        List<Step> availableSteps = new ArrayList<>();

        steps.forEach((s, step) -> {
            boolean bound = false;
            for(int i = 0; i < step.getRules().size(); i++) {
                if(step.getRules().get(i).getAfter().equals(s))
                    bound = true;
            }

            if(!bound && !step.isUsed())
                availableSteps.add(step);
        });

        availableSteps.sort(new Comparator<Step>() {
            @Override
            public int compare(Step step, Step t1) {
                return step.getStep().compareTo(t1.getStep());
            }
        });

        if(availableSteps.size() > 0)
            return availableSteps.get(0);
        else
            return null;
    }

    public static HashMap<String, Step> removeRulesDependingOnStep(HashMap<String, Step> steps, Step step) {
        steps.forEach((stepLetter, s) -> {
            List<Rule> newRules = new ArrayList<>();
            for(int i = 0; i < s.getRules().size(); i++) {
                if(!step.getStep().equals(s.getRules().get(i).getBefore())) {
                    newRules.add(s.getRules().get(i));
                }
            }

            steps.get(stepLetter).setRules(newRules);
        });

        return steps;
    }

}