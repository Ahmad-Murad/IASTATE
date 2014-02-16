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

        stack.push(pivotInd);
        stack.push(right);

        while (stack.size() > 0)
        {
            rightSub = stack.pop();
            leftSub = stack.pop();

            left = leftSub + 1;
            pivotInd = leftSub;
            right = rightSub;
            pivotVal = list.get(pivotInd);

            if (left > right)
                continue;

            while (left < right)
            {
                while ((left <= right) && (list.get(left).compareTo(pivotVal) != 1))
                    left++;

                while ((left <= right) && (list.get(right).compareTo(pivotVal) != -1))
                    right--;

                if (right >= left) {
                    // need to swap
                    E temp = list.get(right);
                    list.set(right, list.get(left));
                    list.set(left, temp);
                }
            }

            if (pivotInd <= right && list.get(pivotInd).compareTo(list.get(right)) == 1) {
                // need to swap
                E temp = list.get(right);
                list.set(right, list.get(pivotInd));
                list.set(pivotInd, temp);
            }

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
//    private void doQuickSort(ArrayList<E> list, int left, int right) {
//        int pivotIndex = rand.nextInt(list.size());
//        E pivotValue = list.get(pivotIndex);
//        System.out.println("pivot is: " + pivotValue.toString() + " at index: " + pivotIndex);
//        E largest1 = null;
//        E largest2 = null;
//
//        while (left < right) {
//            // select element: arr[left] >= pivot
//            while (left < right && list.get(left).compareTo(pivotValue) == -1) {
//                left++;
//
//                // check for largest and 2nd largest element
//                if (largest1 == null || list.get(right).compareTo(largest1) >= 0) {
//                    largest2 = largest1;
//                    largest1 = list.get(right);
//                }
//                else if (largest2 == null || list.get(right).compareTo(largest2) == 1) {
//                    largest2 = list.get(right);
//                }
//            }
//
//            // select element: arr[right] <= pivot
//            while (left < right && list.get(right).compareTo(pivotValue) == 1) {
//                right--;
//            }
//
//            // check for largest and 2nd largest element
//            if (largest1 == null || list.get(right).compareTo(largest1) >= 0) {
//                largest2 = largest1;
//                largest1 = list.get(right);
//            }
//            else if (largest2 == null || list.get(right).compareTo(largest2) == 1) {
//                largest2 = list.get(right);
//            }
//
//            // swap the elements
//            E temp = list.get(left);
//            list.set(left++, list.get(right));
//            list.set(right--, temp);
//        }
//        System.out.println("Largest1: " + largest1 + " Largest2: " + largest2);
//    }
}
