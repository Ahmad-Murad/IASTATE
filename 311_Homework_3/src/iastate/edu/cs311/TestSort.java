/**
 * 
 */
package iastate.edu.cs311;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Random;

import junit.framework.TestResult;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * @author aguibert
 * 
 */
public class TestSort
{
    public enum TYPE {
        INCREASING,
        DECREASING,
        SAME,
        RANDOM,
        ALTERNATING
    }

    private static final int testSize = 13000;
    private static InsertionSort<Integer> is = new InsertionSort<Integer>();
    private static QuickSort<Integer> qs = new QuickSort<Integer>();
    private static MergeSort<Integer> ms = new MergeSort<Integer>();

    @Rule
    public TestName tn = new TestName();
    public TestResult tr = new TestResult();

    @BeforeClass
    public static void beforeAll() {
        System.out.println("Testing sorts with array size: N=" + testSize);
    }

    @Before
    public void doBefore() {
        System.out.println(tn.getMethodName() + " starting:");
    }

    @Test
    public void test_IS_Inc() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.INCREASING);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_QS_Inc() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.INCREASING);
        qs.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_MS_Inc() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.INCREASING);
        ms.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_IS_Dec() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.DECREASING);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_QS_Dec() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.DECREASING);
        qs.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_MS_Dec() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.DECREASING);
        ms.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_IS_Rand() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.RANDOM);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_QS_Rand() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.RANDOM);
        qs.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_MS_Rand() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.RANDOM);
        ms.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_IS_Same() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.SAME);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_QS_Same() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.SAME);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_MS_Same() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.SAME);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_IS_Alt() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        is.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_MS_Alt() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        ms.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @Test
    public void test_QS_Alt() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        qs.analyzeSort(testList);

        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));
        printArray(testList, "Sorted array:  ");
    }

    @SuppressWarnings({ "unchecked" })
    @Test
    public void checkStuffNotDestroyed_QS() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        ArrayList<Integer> verifyList = (ArrayList<Integer>) testList.clone();

        qs.analyzeSort(testList);
        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));

        printArray(testList, "Sorted array:  ");
        printArray(verifyList, "Verify array:  ");

        for (int i = 0; i < testSize; i++) {
            if (!testList.contains(verifyList.get(i))) {
                fail("TestList did NOT contain element: " + verifyList.get(i));
            }
        }
    }

    @SuppressWarnings({ "unchecked" })
    @Test
    public void checkStuffNotDestroyed_IS() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        ArrayList<Integer> verifyList = (ArrayList<Integer>) testList.clone();

        is.analyzeSort(testList);
        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));

        printArray(testList, "Sorted array:  ");
        printArray(verifyList, "Verify array:  ");

        for (int i = 0; i < testSize; i++) {
            if (!testList.contains(verifyList.get(i))) {
                fail("TestList did NOT contain element: " + verifyList.get(i));
            }
        }
    }

    @SuppressWarnings({ "unchecked" })
    @Test
    public void checkStuffNotDestroyed_MS() {
        ArrayList<Integer> testList = fillArr(testSize, TYPE.ALTERNATING);
        ArrayList<Integer> verifyList = (ArrayList<Integer>) testList.clone();

        ms.analyzeSort(testList);
        assertTrue("Array list was not sorted:\n" + testList.toString(), isSorted(testList));

        printArray(testList, "Sorted array:  ");
        printArray(verifyList, "Verify array:  ");

        for (int i = 0; i < testSize; i++) {
            if (!testList.contains(verifyList.get(i))) {
                fail("TestList did NOT contain element: " + verifyList.get(i));
            }
        }
    }

    public static ArrayList<Integer> fillArr(int size, TYPE fillType) {
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
            Random rand = new Random(size + System.nanoTime());
            for (int i = 0; i < size; i++) {
                list.add(new Integer(rand.nextInt() % testSize));
            }
        }
        else if (TYPE.ALTERNATING == fillType) {
            for (int i = 0; i < size; i++) {
                if (i % 2 == 1) {
                    list.add(new Integer(i * (-1)));
                } else {
                    list.add(new Integer(size - i));
                }
            }
        }
        else {
            throw new RuntimeException("Invalid fillType of: " + fillType);
        }

        printArray(list, "Created array: ");
        return list;
    }

    public static boolean isSorted(ArrayList<Integer> list) {
        if (list.size() != testSize) {
            printArray(list, "Array size was incorrect: ");
            return false;
        }

        for (int i = 0; i < list.size() - 1; i++) {
            if (list.get(i).compareTo(list.get(i + 1)) == 1) {
                printArray(list, "Array NOT sorted: ");
                return false;
            }
        }
        return true;
    }

    private static void printArray(ArrayList<Integer> list, String msg) {
        if (list.size() > 100)
            return;
        System.out.println('\t' + msg + list.toString());
    }
}
