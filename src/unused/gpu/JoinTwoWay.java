package ilp.gpu;

import com.aparapi.Kernel;

public class JoinTwoWay extends Kernel {
    public int[] table1;
    public int[] table2;
    public int[] result1;
    public int[] result2;
    public int colLength1;
    public int colLength2;
    public int[] joinCols;
    public int[][] constraintCols;
    public int[] constraintColSize;

    public int rowCount1;
    public int rowCount2;
    public int[][] rows;


    public int[][] values1;
    public int[][] values2;

    public Kernel init() {
        result1 = new int[rowCount1 * colLength1];
        result2 = new int[rowCount2 * colLength2];
        for (int i = 0; i < result1.length; i++) {
            result1[i] = -1;
        }
        for (int i = 0; i < result2.length; i++) {
            result2[i] = -1;
        }

        rows = new int[rowCount1][rowCount2];

        for (int i = 0; i < rowCount1; i++) {
            for (int j = 0; j < rowCount2; j++) {
                rows[i][j] = -1;
            }
        }
        return this;
    }

    public boolean constraints(int rw1, int rw2) {
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

        return true;
    }

    public void values(int rw1, int rw2){
        int table1RowStart = rw1 * colLength1;
        int table2RowStart = rw2 * colLength2;

        for (int i=0; i < colLength1; i++){
            int value = table1[table1RowStart + i];
            result1[table1RowStart + i] = value;
        }

        for (int i=0; i < colLength2; i++){
            int value = table2[table2RowStart + i];
            result2[table2RowStart + i] = value;
        }

        rows[rw1][rw2] = 1;

    }



    @Override
    public void run() {
        int rw1 = getGlobalId(0);
        int rw2 = getGlobalId(1);
        int table1RowStart = rw1 * colLength1;
        int table1Key = table1[table1RowStart + joinCols[0]];
        int table2RowStart = rw2 * colLength2;
        int table2Key = table2[table2RowStart + joinCols[1]];

        if (table1Key == table2Key && constraints(rw1, rw2)) {
            values(rw1, rw2);
        }
    }

    public void runFlat() {

        for (int rw1 = 0; rw1 < rowCount1; rw1++) { // Thread ID
            int table1RowStart = rw1 * colLength1;
            int table1Key = table1[table1RowStart + joinCols[0]];

            for (int rw2 = 0; rw2 < rowCount2; rw2++) {
                int table2RowStart = rw2 * colLength2;
                int table2Key = table2[table2RowStart + joinCols[1]];
                if (table1Key == table2Key && constraints(rw1, rw2)) {
                    values(rw1, rw2);
                }
            }
        }
    }
}
