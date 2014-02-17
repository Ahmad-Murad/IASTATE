/**
 * 
 */
package iastate.edu.cs311;

import java.util.ArrayList;

/**
 * @author aguibert
 * 
 */
public class InsertionSort<E extends Comparable<? super E>> implements SortAnalysis<E> {

    public InsertionSort() {}

    @Override
    public int analyzeSort(ArrayList<E> list) {
        long startTime = System.currentTimeMillis();
        doInsertionSort(list);
        long endTime = System.currentTimeMillis();
        return (int) (endTime - startTime);
    }

    private void doInsertionSort(ArrayList<E> list) {
        for (int i = 0; i < list.size() - 1; i++)
        {
            // check if element to the right is > current element
            if (list.get(i).compareTo(list.get(i + 1)) == 1)
            {
                E elementToInsert = list.get(i + 1);
                // need to insert in sorted array... look backwards
                int j = i;
                while (j >= 0 && elementToInsert.compareTo(list.get(j)) == -1) {
                    // slide j to the right
                    list.set(j + 1, list.get(j));
                    j--;
                }
                list.set(j + 1, elementToInsert);
            }
        }
    }
}
