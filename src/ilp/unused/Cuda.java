package ilp.data.database;


import com.aparapi.Kernel;

public class Cuda extends Kernel {
    private int[] rows;
    private int[][] new_rows;
    private int[][] result;
    private final int size;
    public Cuda(int[] rows, int[][] new_rows, int size) {
        this.rows = rows;
        this.new_rows = new_rows;
        this.size = size;

        this.setExplicit(true);
    }

    public Cuda setRows(int[] rows){
        this.rows = rows;
        put(this.rows);
        return this;
    }

    public Cuda setNewRows(int[][] newRows){
        this.new_rows = newRows;
        put(this.new_rows);
        return this;
    }

    public Cuda init(){
        this.result = new int[new_rows.length][size];
        return this;
    }

    @Override
    public void run(){
        int valueId = getGlobalId(0);
        int rowId = getGlobalId(1);
        this.result[valueId][rowId] = this.rows[rowId] & this.new_rows[valueId][rowId];
    }

    public void runFlat(){
        for (int valueId=0; valueId < result.length; valueId++) {
            for(int rowId=0; rowId < size; rowId++) {
               this.result[valueId][rowId] = this.rows[rowId] & this.new_rows[valueId][rowId];
            }
        }
    }

    public int[][] getResult() {
        get(result);
        return result;
    }
}
