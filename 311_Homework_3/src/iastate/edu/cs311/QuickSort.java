/**
 * 
 */
package iastate.edu.cs311;

import java.util.ArrayList;
import java.util.Stack;

/**
 * @author aguibert
 * 
 */
public class QuickSort<E extends Comparable<? super E>> implements SortAnalysis<E> {

    public QuickSort() {}

    @Override
    public int analyzeSort(ArrayList<E> list) {
        long startTime = System.currentTimeMillis();
        doQuickSort(list);
        long endTime = System.currentTimeMillis();
        return (int) (endTime - startTime);
    }

    private void doQuickSort(ArrayList<E> list)
    {
        Stack<Integer> stack = new Stack<Integer>();
        Integer pivotInd = 0, left = pivotInd + 1, right = list.size() - 1, leftSub, rightSub;
        E pivotVal;

        // Stack will be ordered: [top] right/left  ...  right/left [bottom]
        stack.push(pivotInd);
        stack.push(right);

        while (stack.size() > 0)
        {
            // Grab sub-indexes off the stack in correct order (right then left)
            rightSub = stack.pop();
            leftSub = stack.pop();

            left = leftSub + 1;
            right = rightSub;

            // if the left boundary overlaps the right boundary, this subsection is sorted
            if (left > right)
                continue;

            // choose pivot as leftmost element in subarray
            pivotInd = leftSub;
            pivotVal = list.get(pivotInd);

            while (left < right)
            {
                // find an element which is <= pivot
                while ((left <= right) && (list.get(left).compareTo(pivotVal) != 1))
                    left++;

                // find an element which is >= pivot
                while ((left <= right) && (list.get(right).compareTo(pivotVal) != -1))
                    right--;

                // if we have two different elements on the incorrect side of pivot, swap them
                if (right >= left) {
                    E temp = list.get(right);
                    list.set(right, list.get(left));
                    list.set(left, temp);
                }
            }

            // get the pivot index to the correct spot if left/right iteration terminated early
            if (pivotInd <= right && list.get(pivotInd).compareTo(list.get(right)) == 1) {
                E temp = list.get(right);
                list.set(right, list.get(pivotInd));
                list.set(pivotInd, temp);
            }

            // if there are additional subarrays that need to be sorted,
            // then push the subarray endpoint indexes to the stack
            if (leftSub < right) {
                stack.push(leftSub);
                stack.push(right - 1);
            }
            if (rightSub > right) {
                stack.push(right + 1);
                stack.push(rightSub);
            }
        }
    }
}
