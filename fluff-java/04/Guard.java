import java.util.HashMap;

class Guard {
    private int id;
    private HashMap<Integer, Integer> sleepPerMinute;

    public Guard(int id) {
        this.id = id;
        sleepPerMinute = new HashMap<>();
        for(int i = 0; i < 60; i++) {
            sleepPerMinute.put(i, 0);
        }
    }

    public void addSleepToMinute(int minute) {
        this.sleepPerMinute.replace(minute, this.sleepPerMinute.get(minute) + 1);
    }

    public void addSleep(int asleep, int awake) {
        for(int i = asleep; i < awake; i++) {
            this.addSleepToMinute(i);
        }
    }

    public int getMaxSleepMinute() {
        return this.sleepPerMinute.entrySet().stream().max((m1, m2) -> m1.getValue() > m2.getValue() ? 1 : -1).get().getKey();
    }

    public int getSleepCount(int minute) {
        return this.sleepPerMinute.get(minute);
    }

    public int getTotalSleep() {
        return (int) this.sleepPerMinute.values().stream().mapToLong(v -> v).sum();
    }
}