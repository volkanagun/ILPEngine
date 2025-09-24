package ilp.invent;

import ilp.data.program.Hypothesis;

import java.io.Serializable;
import java.util.List;

public class TaskResult implements Serializable {
    Hypothesis[] set;
    Hypothesis[] array;

    public TaskResult(Hypothesis[] set, Hypothesis[] array) {
        this.set = set;
        this.array = array;
    }

    public Hypothesis[] getSet() {
        return set;
    }

    public void setSet(Hypothesis[] set) {
        this.set = set;
    }

    public Hypothesis[] getArray() {
        return array;
    }

    public void setArray(Hypothesis[] array) {
        this.array = array;
    }
}
