package ilp.cpu;


import com.aparapi.Kernel;

public class GPUBatchFilter extends Kernel {

    private int[] positions;
    private int[][][] results;
    private int[][] rows;
    private int[] values;
    private int valueSize;
    private int rowMax;
    private int tableSize;

    private int[][] tables;

    private int[] rowsize;
    private int[] colsize;

    public GPUBatchFilter(int[][] tables, int[][] rows, int[] rowsize, int[] colsize, int[] positions, int[] values, int rowMax, int localSize) {
        this.tables = tables;
        this.rows = rows;
        this.rowsize = rowsize;
        this.colsize = colsize;
        this.positions = positions;
        this.values = values;
        this.rowMax = rowMax;
        this.tableSize = positions.length;
        this.valueSize = values.length;
        init();
    }

    public GPUBatchFilter copy(){
        return new GPUBatchFilter(tables, rows, rowsize, colsize, positions, values, rowMax, 1);
    }

    public GPUBatchFilter init(){
        this.results = new int[valueSize][tableSize][rowMax];
        this.setExplicit(true);

        return this;
    }

    public int[][][] getResults() {
        get(results);
        return results;
    }

    public void setRowMax(int rowMax) {
        this.rowMax = rowMax;
    }

    public void setRowsize(int[] rowsize) {
        this.rowsize = rowsize;
    }

    public void setColsize(int[] colsize) {
        this.colsize = colsize;
    }

    public void setPositions(int[] positions) {
        this.positions = positions;
        this.tableSize = positions.length;
        put(positions);
    }

    public void setRows(int[][] rows) {
        this.rows = rows;
        put(this.rows);
    }

    public void setTables(int[][] data) {
        this.tables = data;
        put(tables);
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

        int rowIndex = getGlobalId(0);
        int valueIndex = getGlobalId(1);
        int tableIndex = getGlobalId(2);

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
            for (int tableIndex = 0; tableIndex < tableSize; tableIndex++) {
                for (int rowIndex = 0; rowIndex < rowMax; rowIndex++) {

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

