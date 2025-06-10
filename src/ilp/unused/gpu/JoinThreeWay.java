package ilp.gpu;

import com.aparapi.Kernel;

public class JoinThreeWay extends Kernel {
    public int[] table1; // Flattened table 1
    public int[] table2; // Flattened table 2
    public int[] table3; // Flattened table 2
    public int[] result; // Output array to store matching row indices
    public int colLength1; // Number of columns in table 1
    public int colLength2; // Number of columns in table 2
    public int colLength3; // Number of columns in table 2
    public int[] joinCols; // Join column index for table 1
    public int rowCount1; // Number of rows in table 1
    public int rowCount2; // Number of rows in table 2
    public int rowCount3; // Number of rows in table 2

    public int[][] constraintCols;
    public int[] constraintColSize;

    public int[][] values1;
    public int[][] values2;
    public int[][] values3;

    public int[] rows1;
    public int[] rows2;
    public int[] rows3;

    public Kernel init() {
        result = new int[rowCount1 * rowCount2 * rowCount3 * 3];
        return this;
    }

    public void values(int rw1, int rw2, int rw3){
        int table1RowStart = rw1 * colLength1;
        int table2RowStart = rw2 * colLength2;
        int resultIndex = (rw1 * rowCount2 * rowCount3 + rw2 * rowCount3 + rw3) * 3;

        for (int i=0; i < colLength1; i++){
            result[resultIndex] = table1[table1RowStart + i];
            resultIndex = resultIndex + 1;
        }

        for (int i=0; i < colLength2; i++){
            result[resultIndex] = table2[table2RowStart + i];
            resultIndex = resultIndex + 1;
        }

        for (int i=0; i < colLength3; i++){
            result[resultIndex] = table3[table2RowStart + i];
            resultIndex = resultIndex + 1;
        }

    }

    public boolean constraints(int rw1, int rw2, int rw3) {
        int table1RowStart = rw1 * colLength1;
        int table1Size = constraintColSize[0];
        for (int j = 0; j < table1Size; j++) {
            int column = constraintCols[0][j];
            int table1Key = table1[table1RowStart + column];
            if (values1[column][table1Key] == -1) return false;
        }

        int table2RowStart = rw2 * colLength2;
        int table2Size = constraintColSize[1];
        for (int j = 0; j < table2Size; j++) {
            int column = constraintCols[1][j];
            int table2Key = table2[table2RowStart + column];
            if (values2[column][table2Key] == -1) return false;
        }

        int table3RowStart = rw3 * colLength3;
        int table3Size = constraintColSize[2];
        for (int j = 0; j < table3Size; j++) {
            int column = constraintCols[2][j];
            int table3Key = table3[table3RowStart + column];
            if (values3[column][table3Key] == -1) return false;
        }

        return true;
    }

    @Override
    public void run() {
        int rw1 = getGlobalId(0);
        int rw2 = getGlobalId(1);
        int rw3 = getGlobalId(2);

        int table1RowStart = rw1 * colLength1;
        int table2RowStart = rw2 * colLength2;
        int table3RowStart = rw3 * colLength3;

        int table1Key = table1[table1RowStart + joinCols[0]];
        int table2Key = table2[table2RowStart + joinCols[1]];
        int table3Key = table3[table3RowStart + joinCols[2]];

        if (table1Key == table2Key && table3Key == table1Key && constraints(rw1, rw2, rw3)) {
            values(rw1, rw2, rw3);
        }

    }
}