package ilp.cpu;


import com.aparapi.Kernel;

public class GPUBatchFilter extends Kernel {

    private int[] positions;
    private int[][][] results;
    private int[][] rows;
    private int[] values;
    private int valueSize;
    @Constant
    private int size;
    @Constant
    private int predicateSize;
    @Constant
    private int[][] tables;
    @Constant
    private int[] rowsize;
    @Constant
    private int[] colsize;

    public GPUBatchFilter(int[][] tables, int[][] rows, int[] rowsize, int[] colsize, int[] positions, int[] values, int size) {
        this.tables = tables;
        this.rows = rows;
        this.rowsize = rowsize;
        this.colsize = colsize;
        this.positions = positions;
        this.values = values;
        this.size = size;
        this.predicateSize = positions.length;
        this.valueSize = values.length;
        init();
    }

    public GPUBatchFilter init(){
        this.results = new int[valueSize][predicateSize][size];
        this.setExplicit(true);

        return this;
    }

    public int[][][] getResults() {
        get(results);
        return results;
    }

    public void setPositions(int[] positions) {
        this.positions = positions;
        put(positions);
    }

    public void setRows(int[][] rows) {
        this.rows = rows;
        put(rows);
    }

    public void setValues(int[] values) {
        this.values = values;
        put(values);
    }

    public void setValueSize(int valueSize) {
        this.valueSize = valueSize;
    }

    @Override
    public void run() {
        int tableIndex = getGlobalId(0);
        int rowIndex = getGlobalId(1);
        int valueIndex = getGlobalId(2);

        if (rowsize[tableIndex] > rowIndex && positions[tableIndex] != -1 && rows[tableIndex][rowIndex] == 1){
            int index = colsize[tableIndex] * rowIndex + positions[tableIndex];
            if(tables[tableIndex][index] == values[valueIndex]) results[valueIndex][tableIndex][rowIndex] = 1;
        }
        else if(positions[tableIndex] == -1 && rows[tableIndex][rowIndex] == 1){
            results[valueIndex][tableIndex][rowIndex] = 1;
        }
    }

    public void runFlat() {
        for (int valueIndex = 0; valueIndex < values.length; valueIndex++) {
            for (int tableIndex = 0; tableIndex < predicateSize; tableIndex++) {
                for (int rowIndex = 0; rowIndex < size; rowIndex++) {

                    if (rowsize[tableIndex] > rowIndex && positions[tableIndex] != -1 && rows[tableIndex][rowIndex] == 1){
                        int index = colsize[tableIndex] * rowIndex + positions[tableIndex];
                        if(tables[tableIndex][index] == values[valueIndex]) results[valueIndex][tableIndex][rowIndex] = 1;
                    }
                    else if(positions[tableIndex] == -1 && rows[tableIndex][rowIndex] == 1){
                        results[valueIndex][tableIndex][rowIndex] = 1;
                    }
                }
            }
        }
    }

}

