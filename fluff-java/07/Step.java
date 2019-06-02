import java.util.Objects;
import java.util.List;

class Step {

    private String step;
    private List<Rule> rules;
    private boolean used;

    public Step(String step, List<Rule> rules) {
        this.step = step;
        this.rules = rules;
    }

    public String getStep() {
        return step;
    }

    public void setStep(String step) {
        this.step = step;
    }

    public List<Rule> getRules() {
        return rules;
    }

    public void setRules(List<Rule> rules) {
        this.rules = rules;
    }

    public boolean isUsed() {
        return used;
    }

    public void setUsed(boolean used) {
        this.used = used;
    }

    @Override
    public String toString() {
        return "Step{" +
                "step='" + step + '\'' +
                ", rules=" + rules +
                ", used=" + used +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Step step1 = (Step) o;
        return Objects.equals(step, step1.step);
    }

    @Override
    public int hashCode() {
        return Objects.hash(step);
    }
}