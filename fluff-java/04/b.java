import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Day04B {

    public static void main(String[] args) {
        Pattern pattern = Pattern.compile("\\[(\\d{4}\\-\\d\\d\\-\\d\\d\\ \\d\\d\\:\\d\\d)\\]\\ (.*)");
        Pattern guardIdPattern = Pattern.compile("Guard\\ \\#(\\d+).*");

        Scanner in = new Scanner(System.in);

        ArrayList<String> events = new ArrayList<>();
        HashMap<Integer, Guard> guardHashMap = new HashMap<>();

        while (in.hasNext()) {
            events.add(in.nextLine());
        }
        in.close();

        Collections.sort(events);

        int id = 0;
        int asleep = -1;
        int awake = -1;
        for(String event : events) {
            Matcher matcher = pattern.matcher(event);
            if(matcher.matches()) {
                String ts = matcher.group(1);
                String text = matcher.group(2);
                Matcher guardMatcher = guardIdPattern.matcher(text);
                if(guardMatcher.matches()) {
                    id = Integer.valueOf(guardMatcher.group(1));
                }
                else if(text.startsWith("falls")) {
                    asleep = Integer.valueOf(ts.substring(ts.length() - 2));
                }
                else if(text.startsWith("wakes")) {
                    awake = Integer.valueOf(ts.substring(ts.length() - 2));
                }
            }

            if(id > 0 && asleep > -1 && awake > -1) {
                Guard guard = new Guard(id);
                if(guardHashMap.containsKey(id))
                    guard = guardHashMap.get(id);

                guard.addSleep(asleep, awake);
                guardHashMap.put(id, guard);
                asleep = -1;
                awake = -1;
            }
        }

        int guardId = guardHashMap.entrySet()
                .stream()
                .max((g1, g2) -> g1.getValue().getSleepCount(g1.getValue().getMaxSleepMinute()) > g2.getValue().getSleepCount(g2.getValue().getMaxSleepMinute()) ? 1 : -1)
                .get()
                .getKey();

        Guard guard = guardHashMap.get(guardId);

        System.out.println("Guard " + guardId + " sleep " + guard.getSleepCount(guard.getMaxSleepMinute()) + " times during minute " + guard.getMaxSleepMinute());


    }
}