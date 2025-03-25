package ilp.cpu;

import com.aparapi.Kernel;

public class GPUFilter extends Kernel {

    public int[] positions;
    public int[][][] tables;
    public int[][] results;
    public int[] rowSize;

    public int value;
    public int tableSize;
    public int maxRowSize;

    public GPUFilter() {
    }

    public GPUFilter(int[][][] tables, int[] positions, int[] rowSize, int maxRowSize, int value) {
        this.tables = tables;
        this.positions = positions;
        this.rowSize = rowSize;
        this.tableSize = positions.length;
        this.maxRowSize = maxRowSize;
        this.results = new int[tableSize][maxRowSize];
        this.value = value;
    }


    @Override
    public void run() {
        int tableIndex = getGlobalId(0);
        int tableRowIndex = getGlobalId(1);

        if (rowSize[tableIndex] > tableRowIndex) {
            if (tables[tableIndex][tableRowIndex][positions[tableIndex]] == value) {
                results[tableIndex][tableRowIndex] = 1;
            }
        }
    }

    public void runTest(int tableIndex, int tableRowIndex) {
        if (rowSize[tableIndex] > tableRowIndex && tables[tableIndex][tableRowIndex][positions[tableIndex]] == value)
        {
            results[tableIndex][tableRowIndex] = 1;
        }
    }

    public void runFlat() {
        for (int predicateIndex = 0; predicateIndex< tableSize; predicateIndex++) {
            for (int tableRowIndex = 0; tableRowIndex < maxRowSize; tableRowIndex++) {
                runTest(predicateIndex, tableRowIndex);
            }
        }
    }

}
