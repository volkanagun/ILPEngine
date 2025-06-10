package ilp.gpu;

import com.aparapi.Kernel;

public class JoinFourWay extends Kernel {
    public int[] table1; // Flattened table 1
    public int[] table2; // Flattened table 2
    public int[] table3; // Flattened table 2
    public int[] table4; // Flattened table 2
    public int[] result; // Output array to store matching row indices
    public int colLength1; // Number of columns in table 1
    public int colLength2; // Number of columns in table 2
    public int colLength3; // Number of columns in table 2
    public int colLength4; // Number of columns in table 2
    public int[] joinCols; // Join column index for table 1
    public int rowCount1; // Number of rows in table 1
    public int rowCount2; // Number of rows in table 2
    public int rowCount3; // Number of rows in table 2
    public int rowCount4; // Number of rows in table 2

    public int[] rowFilter1;
    public int[] rowFilter2;
    public int[] rowFilter3;
    public int[] rowFilter4;

    public Kernel init(){
        result = new int[rowCount1 * rowCount2 * rowCount3 * rowCount4 * 4];
        return this;
    }

    @Override
    public void run() {
        int r1 = getGlobalId(0);
        int r2 = getGlobalId(1);
        int r3 = getGlobalId(2);
        int r4 = getLocalId(0);


        // Iterate over rows in table 1

        if (rowFilter1[r1] != -1 && rowFilter2[r2] != -1 && rowFilter3[r3] != -1 && rowFilter4[r4] != -1) {

            int table1RowStart = r1 * colLength1;
            int table2RowStart = r2 * colLength2;
            int table3RowStart = r3 * colLength3;
            int table4RowStart = r4 * colLength4;

            int table1Key = table1[table1RowStart + joinCols[0]];
            int table2Key = table2[table2RowStart + joinCols[1]];
            int table3Key = table3[table3RowStart + joinCols[2]];
            int table4Key = table4[table4RowStart + joinCols[3]];

            if (table1Key == table2Key && table3Key == table1Key && table4Key == table1Key) {
                int resultIndex = (r1 * rowCount2 * rowCount3 + r2 * rowCount3 + r3 * rowCount4 + r4) * 4;
                result[resultIndex] = r1;
                result[resultIndex + 1] = r2;
                result[resultIndex + 2] = r3;
                result[resultIndex + 3] = r4;
            }
        }

    }
}