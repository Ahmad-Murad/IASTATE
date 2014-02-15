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
        List<E> _list = list;
        long startTime = System.currentTimeMillis();
        doMergeSort(_list);
        long endTime = System.currentTimeMillis();
        return (int) (endTime - startTime);
    }

    private void doMergeSort(List<E> list) {
        // validate input
        if (list == null || list.size() < 1)
            return;

        // split array in half
        int middle = list.size() / 2;
        List<E> leftHalf = list.subList(0, middle);
        List<E> rightHalf = list.subList(middle, list.size() - 1);

        // recursively sort the two halves
        doMergeSort(leftHalf);
        doMergeSort(rightHalf);

        // now merge the sorted subarrays
        list = doMerge(leftHalf, rightHalf);
    }

    private List<E> doMerge(List<E> left, List<E> right)
    {
        List<E> merged = new ArrayList<E>(left.size() + right.size());
        int leftIndex = 0, rightIndex = 0;
        for (int i = 0; i < merged.size(); i++)
        {
            if (leftIndex < left.size() && left.get(leftIndex).compareTo(right.get(rightIndex)) != 1) {
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
