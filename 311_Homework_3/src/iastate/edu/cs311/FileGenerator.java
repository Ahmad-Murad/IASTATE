/**
 * 
 */
package iastate.edu.cs311;

import iastate.edu.cs311.TestSort.TYPE;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

/**
 * @author Andrew
 * 
 */
public class FileGenerator {

    private static InsertionSort<Integer> is = new InsertionSort<Integer>();
    private static QuickSort<Integer> qs = new QuickSort<Integer>();
    private static MergeSort<Integer> ms = new MergeSort<Integer>();

    public static void main(String args[])
    {
        testAndWriteToCSV("mrg_sam", ms, TYPE.SAME);
    }

    private static void testAndWriteToCSV(String fileName, SortAnalysis<Integer> sorter, TYPE type)
    {
        if (fileName == null)
            throw new RuntimeException("File name cannot be null");

        fileName = "output/" + fileName + ".csv";

        FileWriter csv = null;
        try {
            csv = new FileWriter(fileName);

            csv.append("Array Size, time(ms)\n");

            for (int size = 1000; size <= 100000; size += 1000) {
                ArrayList<Integer> list = fillArr(size, type);
                long time = sorter.analyzeSort(list);
                checkSorted(list);
                csv.append(size + " , ");
                csv.append(time + "\n");
            }

        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        } finally {
            try {
                if (csv != null) {
                    csv.flush();
                    csv.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private static ArrayList<Integer> fillArr(int size, TYPE fillType) {
        ArrayList<Integer> list = new ArrayList<Integer>(size);

        if (TYPE.INCREASING == fillType) {
            for (int i = 0; i < size; i++)
                list.add(new Integer(i));
        }
        else if (TYPE.DECREASING == fillType) {
            for (int i = 0; i < size; i++)
                list.add(new Integer(size - i));
        }
        else if (TYPE.SAME == fillType) {
            for (int i = 0; i < size; i++)
                list.add(new Integer(size));
        }
        else if (TYPE.RANDOM == fillType) {
            Random rand = new Random(size);
            for (int i = 0; i < size; i++) {
                list.add(new Integer(rand.nextInt()));
            }
        }
        else {
            throw new RuntimeException("Invalid fillType of: " + fillType);
        }

        if (size < 100)
            System.out.println("Created array: " + list.toString());

        return list;
    }

    private static void checkSorted(ArrayList<Integer> list) {
        for (int i = 0; i < list.size() - 1; i++) {
            if (list.get(i).compareTo(list.get(i + 1)) == 1) {
                if (list.size() < 200) {
                    System.out.println("Array NOT sorted: " + list.toString());
                }
                throw new RuntimeException("Array NOT Sorted");
            }
        }
        return;
    }
}
