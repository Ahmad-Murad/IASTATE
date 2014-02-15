/**
 * 
 */
package iastate.edu.cs311;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Random;

import org.junit.Test;

/**
 * @author aguibert
 * 
 */
public class TestSort
{
    private enum TYPE {
        INCREASING,
        DECREASING,
        SAME,
        RANDOM
    }

    private final int testSize = 5;
    //private static InsertionSort<Integer> is = new InsertionSort<Integer>();
    //private static QuickSort<Integer> is = new QuickSort<Integer>();
    private static MergeSort<Integer> is = new MergeSort<Integer>();

    @Test
    public void testIncreasing() {
        ArrayList<Integer> increasingList = fillArr(testSize, TYPE.INCREASING);
        long time = is.analyzeSort(increasingList);

        assertTrue("Array list was not sorted:\n" + increasingList.toString(), isSorted(increasingList));
    }

    @Test
    public void testDecreasing() {
        ArrayList<Integer> increasingList = fillArr(testSize, TYPE.DECREASING);
        long time = is.analyzeSort(increasingList);

        assertTrue("Array list was not sorted:\n" + increasingList.toString(), isSorted(increasingList));
    }

    @Test
    public void testRandom() {
        ArrayList<Integer> increasingList = fillArr(testSize, TYPE.RANDOM);
        long time = is.analyzeSort(increasingList);

        assertTrue("Array list was not sorted:\n" + increasingList.toString(), isSorted(increasingList));
    }

    @Test
    public void testSame() {
        ArrayList<Integer> increasingList = fillArr(testSize, TYPE.SAME);
        long time = is.analyzeSort(increasingList);

        assertTrue("Array list was not sorted:\n" + increasingList.toString(), isSorted(increasingList));
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

    private static boolean isSorted(ArrayList<Integer> list) {
        for (int i = 0; i < list.size() - 1; i++) {
            if (list.get(i).compareTo(list.get(i + 1)) == 1) {
                if (list.size() < 200) {
                    System.out.println("Array NOT sorted: " + list.toString());
                }
                return false;
            }
        }
        return true;
    }
}
