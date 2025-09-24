package ilp.tests;

import java.io.Serializable;
import java.util.concurrent.Callable;

public class SquareTask implements Callable<Integer>, Serializable {
    private int n;

    public SquareTask(int n) {
        this.n = n;
    }

    public SquareTask() {
    }

    public int getN() {
        return n;
    }

    public void setN(int n) {
        this.n = n;
    }

    @Override
    public Integer call(){
        return Integer.valueOf(n*n);
    }
}
