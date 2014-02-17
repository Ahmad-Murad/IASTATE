/**
 * 
 */
package iastate.edu.cs311;

import iastate.edu.cs311.TestSort.TYPE;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

/**
 * @author Andrew
 * 
 */
public class FileGenerator {

    private static InsertionSort<Integer> is = new InsertionSort<Integer>();
    private static QuickSort<Integer> qs = new QuickSort<Integer>();
    private static MergeSort<Integer> ms = new MergeSort<Integer>();
    private static final int testSize = 100000;

    public static void main(String args[])
    {
        testAndWriteToCSV("mrg_rnd");
    }

    private static void testAndWriteToCSV(String fileName) {
        SortAnalysis<Integer> sorter;
        TYPE type;

        if (fileName.contains("mrg"))
            sorter = ms;
        else if (fileName.contains("ins"))
            sorter = is;
        else if (fileName.contains("qic"))
            sorter = qs;
        else
            throw new RuntimeException("Bad fileName: " + fileName);

        if (fileName.contains("alt"))
            type = TYPE.ALTERNATING;
        else if (fileName.contains("dec"))
            type = TYPE.DECREASING;
        else if (fileName.contains("inc"))
            type = TYPE.INCREASING;
        else if (fileName.contains("rnd"))
            type = TYPE.RANDOM;
        else
            throw new RuntimeException("Bad fileName: " + fileName);

        System.out.println("Using " + sorter.getClass().getSimpleName() + " with type: " + type);
        testAndWriteToCSV(fileName, sorter, type);
    }

    private static void testAndWriteToCSV(String fileName, SortAnalysis<Integer> sorter, TYPE type)
    {
        if (fileName == null)
            throw new RuntimeException("File name cannot be null");

        fileName = "output/" + fileName + ".csv";

        try (FileWriter csv = new FileWriter(fileName)) {
            csv.append("Array Size, Actual\n");

            for (int size = testSize / 100; size <= testSize; size += (testSize / 100)) {
                ArrayList<Integer> list = TestSort.fillArr(size, type);
                long time = sorter.analyzeSort(list);
                if (!isSorted(list, size))
                    throw new RuntimeException("Array NOT sorted");
                csv.append(size + " , ");
                csv.append(time + "\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        }
    }

    public static boolean isSorted(ArrayList<Integer> list, int expSize) {
        if (list.size() != expSize)
            return false;

        for (int i = 0; i < list.size() - 1; i++) {
            if (list.get(i).compareTo(list.get(i + 1)) == 1) {
                return false;
            }
        }
        return true;
    }
}
