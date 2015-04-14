package ex1_review;

import java.util.Arrays;
import java.util.Random;

/**
 * @author Andrew
 *
 */
public class Main {

    public static void main(String args[]) {
        sortAndTime(100);
        sortAndTime(1000);
        sortAndTime(10000);
        sortAndTime(100000);
    }

    private static long sortAndTime(int SIZE) {
        Random r = new Random();
        int[] arr = new int[SIZE];
        for (int i = 0; i < SIZE; i++)
            arr[i] = r.nextInt(SIZE);

        long start = System.nanoTime();
        Arrays.sort(arr);
        long total = (System.nanoTime() - start) / 1000;
        System.out.println("Sorting time for " + SIZE + " elements was: " + total + " microseconds");
        return total;
    }
}
