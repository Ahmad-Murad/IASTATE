/**
 * 
 */
package iastate.edu.cs311;

import java.util.ArrayList;
import java.util.List;

/**
 * @author aguibert
 * 
 */
public class MergeSort<E extends Comparable<? super E>> implements SortAnalysis<E> {

    public MergeSort() {}

    @Override
    public int analyzeSort(ArrayList<E> list) {
        long startTime = System.currentTimeMillis();
        doMergeSort(list);
        long endTime = System.currentTimeMillis();
        return (int) (endTime - startTime);
    }

    private void doMergeSort(List<E> list) {
        // arrays of size 0 or 1 are already sorted
        if (list == null || list.size() <= 1)
            return;

        // Small optimization, if there are only 2 elements, compare or swap
        // no need to bother with splitting and using more recursion
        if (list.size() == 2 && (list.get(0).compareTo(list.get(1)) == 1)) {
            E temp = list.get(0);
            list.set(0, list.get(1));
            list.set(1, temp);
            return;
        }

        // split array in half
        int middle = list.size() / 2;
        List<E> leftHalf = new ArrayList<E>(list.subList(0, middle));
        List<E> rightHalf = new ArrayList<E>(list.subList(middle, list.size()));

        // recursively sort the two halves
        doMergeSort(leftHalf);
        doMergeSort(rightHalf);

        // now merge the sorted subarrays
        doMerge(list, leftHalf, rightHalf);
    }

    private List<E> doMerge(List<E> merged, List<E> left, List<E> right)
    {
        int leftIndex = 0, rightIndex = 0;
        for (int i = 0; i < merged.size(); i++)
        {
            if (rightIndex >= right.size()
                || (leftIndex < left.size() && left.get(leftIndex).compareTo(right.get(rightIndex)) != 1)) {
                // if left[leftIndex] <= right[rightIndex], choose left
                merged.set(i, left.get(leftIndex));
                leftIndex++;
            }
            else {
                merged.set(i, right.get(rightIndex));
                rightIndex++;
            }
        }

        return merged;
    }
}
