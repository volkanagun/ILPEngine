package ilp.cpu;


import com.aparapi.Kernel;

public class GPUIntersectFilter extends Kernel {

    private int[] positions;
    private int[] results;
    private int[][] rows;
    private int[] values;
    private int valueSize;
    @Constant
    private final int rowMax, localSize;
    @Constant
    private final int tableSize;
    @Constant
    private final int[][] tables;
    @Constant
    private final int[] rowsize;
    @Constant
    private final int[] colsize;

    public GPUIntersectFilter(int[][] tables, int[][] rows, int[] rowsize, int[] colsize, int[] positions, int[] values, int size, int localSize) {
        this.tables = tables;
        this.rows = rows;
        this.rowsize = rowsize;
        this.colsize = colsize;
        this.positions = positions;
        this.values = values;
        this.rowMax = size;
        this.localSize = localSize;
        this.tableSize = positions.length;
        this.valueSize = values.length;
        init();
    }

    public GPUIntersectFilter init() {
        this.results = new int[valueSize];
        this.setExplicit(true);

        return this;
    }

    public int[] getResults() {
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
        this.valueSize = values.length;
        put(values);
    }

    public void setValueSize(int valueSize) {
        this.valueSize = valueSize;
    }

    @Override
    public void run() {

        int rowIndex = getGlobalId(0);
        int valueIndex = getGlobalId(1);
        int count = 0;
        for (int tableIndex = 0; tableIndex < tableSize; tableIndex++) {
            if (rowsize[tableIndex] > rowIndex && positions[tableIndex] != -1 && rows[tableIndex][rowIndex] == 1) {
                int index = colsize[tableIndex] * rowIndex + positions[tableIndex];
                if (tables[tableIndex][index] == values[valueIndex]) {
                    //results[valueIndex][tableIndex][rowIndex] = 1;
                    count++;
                }
            } else if (positions[tableIndex] == -1 && rows[tableIndex][rowIndex] == 1) {
                //results[valueIndex][tableIndex][rowIndex] = 1;
                count++;
            }
        }

        if (count == tableSize){
            results[valueIndex] = 1;
        }
    }


}

