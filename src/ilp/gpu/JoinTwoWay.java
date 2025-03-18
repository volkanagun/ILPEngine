package ilp.gpu;

import com.aparapi.Kernel;

public class JoinTwoWay extends Kernel {
    public int[] table1; // Flattened table 1
    public int[] table2; // Flattened table 2
    public int[] result; // Output array to store matching row indices
    public int colLength1; // Number of columns in table 1
    public int colLength2; // Number of columns in table 2
    public int[] joinCols; // Join column index for table 1
    public int rowCount1; // Number of rows in table 1
    public int rowCount2; // Number of rows in table 2

    public int[] rowFilter1;
    public int[] rowFilter2;

    public Kernel init() {
        result = new int[rowCount1 * rowCount2 * 2];
        for (int i = 0; i < result.length; i++) {
            result[i] = -1;
        }
        return this;
    }

    @Override
    public void run() {
        int rw1 = getGlobalId(0);
        int rw2 = getGlobalId(1);


        // Iterate over rows in table 1
        if (rowFilter1[rw1] != -1 && rowFilter2[rw2] != -1) {
            int table1RowStart = rw1 * colLength1;
            int table1Key = table1[table1RowStart + joinCols[0]];
            int table2RowStart = rw2 * colLength2;
            int table2Key = table2[table2RowStart + joinCols[1]];
            // Check if keys match
            if (table1Key == table2Key) {
                int resultIndex = (rw1 * rowCount2 + rw2) * 2;
                result[resultIndex] = rw1; // Table 1 row index
                result[resultIndex + 1] = rw2; // Table 2 row index
            }
        }
    }

    public void runFlat() {

        for (int rw1 = 0; rw1 < rowCount1; rw1++) { // Thread ID

            // Iterate over rows in table 1
            if (rw1 < rowCount1) {

                if (rowFilter1[rw1] != -1) {
                    int table1RowStart = rw1 * colLength1;
                    int table1Key = table1[table1RowStart + joinCols[0]];

                    // Iterate over rows in table 2
                    for (int rw2 = 0; rw2 < rowCount2; rw2++) {

                        if (rowFilter2[rw2] != -1) {
                            int table2RowStart = rw2 * colLength2;
                            int table2Key = table2[table2RowStart + joinCols[1]];
                            // Check if keys match
                            if (table1Key == table2Key) {
                                // Store matching row indices
                                int resultIndex = (rw1 * rowCount2 + rw2) * 2;
                                result[resultIndex] = rw1; // Table 1 row index
                                result[resultIndex + 1] = rw2; // Table 2 row index
                            }
                        }
                    }
                }
            }
        }
    }
}
