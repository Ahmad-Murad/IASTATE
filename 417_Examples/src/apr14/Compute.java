package apr14;

import java.util.Arrays;

public class Compute {
    public static int median;
    public static int mode;

    public static void median_mode(Integer[] in) {
        Integer[] sorted = sort(in);

        computeMedian(sorted);

        computeMode(sorted);
    }

    /**
     * @param in
     * @return
     */
    private static void computeMedian(Integer[] in) {
        Integer[] sorted = in;
        int middle = -1;
        if (sorted.length % 2 == 0) {
            middle = sorted.length / 2;
            int mid1 = sorted[middle];
            int mid2 = sorted[middle - 1];
            median = getAverage(mid2, mid2);
        } else {
            median = sorted[sorted.length / 2];
        }
    }

    /**
     * @param mid1
     * @param mid2
     * @return
     */
    private static int getAverage(int mid1, int mid2) {
        return (mid1 + mid2) / 2;
    }

    /**
     * @param sorted
     */
    private static void computeMode(Integer[] sorted) {
        int count = 0;
        int maxCount = 0;
        mode = sorted[0];

        int prev = mode;
        for (int i = 0; i < sorted.length; i++) {
            int cur = sorted[i];
            if (cur == prev) {
                count++;
            }
            if (count > maxCount) {
                maxCount = count;
                mode = prev;
            }
            if (cur != prev) {
                count = 1;
            }
            prev = cur;
        }
    }

    private static Integer[] sort(Integer[] in) {
        Arrays.sort(in);
        return in;
    }

    public static void main(String[] args) {
        median_mode(new Integer[] { 3, 4 });
    }
}