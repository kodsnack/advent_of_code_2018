import java.util.Objects;

class Rule {

    private String before;
    private String after;

    public Rule(String before, String after) {
        this.before = before;
        this.after = after;
    }

    public String getBefore() {
        return before;
    }

    public void setBefore(String before) {
        this.before = before;
    }

    public String getAfter() {
        return after;
    }

    public void setAfter(String after) {
        this.after = after;
    }

    @Override
    public String toString() {
        return "Rule{" +
                "before='" + before + '\'' +
                ", after='" + after + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Rule rule = (Rule) o;
        return Objects.equals(before, rule.before) &&
                Objects.equals(after, rule.after);
    }

    @Override
    public int hashCode() {
        return Objects.hash(before, after);
    }
}