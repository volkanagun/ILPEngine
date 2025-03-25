package ilp.cpu;


import com.aparapi.Kernel;

public class GPUBatchFilter extends Kernel {

    private int[] positions;
    private int[][] tables;
    private int[][][] results;

    private int[][] rows;
    private int[] rowsize;
    private int[] colsize;
    private int[] values;
    private int predicateSize;
    private int valueSize;
    private int size;

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
        return this;
    }

    public int[][][] getResults() {
        return results;
    }

    public void setPositions(int[] positions) {
        this.positions = positions;
    }

    public void setRows(int[][] rows) {
        this.rows = rows;
    }

    public void setValues(int[] values) {
        this.values = values;
    }

    public void setValueSize(int valueSize) {
        this.valueSize = valueSize;
    }

    @Override
    public void run() {
        int tableIndex = getLocalId(0);
        int rowIndex = getGlobalId(0);
        int valueIndex = getGlobalId(1);

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
                }
            }
        }
    }

}

