package apr14;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ComputeTest {

    @Test
    public void test() {
        Integer[] in = { 3, 3, 4 };
        Compute.median_mode(in);
        assertTrue(Compute.median == 3);
        assertTrue(Compute.mode == 3);
    }

    @Test
    public void test2() {
        Integer[] in = { 1 };
        Compute.median_mode(in);
        assertTrue(Compute.median == 1);
        assertTrue(Compute.mode == 1);
    }

    @Test
    public void test3() {
        Integer[] in = { 1, 2 };
        Compute.median_mode(in);
        assertTrue(Compute.median == 1);
        assertTrue(Compute.mode == 1);
    }

    @Test
    public void test4() {
        Integer[] in = { 5, 5, 5 };
        Compute.median_mode(in);
        assertTrue(Compute.median == 5);
        assertTrue(Compute.mode == 5);
    }

    @Test
    public void test5() {
        Integer[] in = { 1, 1, 3, 3, 3 };
        Compute.median_mode(in);
        assertTrue(Compute.median == 3);
        assertTrue(Compute.mode == 3);
    }

    @Test
    public void test6() {
        // TODO
        Integer[] in = { 1, 10 };
        Compute.median_mode(in);
        System.out.println(Compute.median);
        System.out.println(Compute.mode);
        assertTrue(Compute.median == 5);
        assertTrue(Compute.mode == 1);
    }

//    @Test
//    public void test7() {
//        Integer[] in = { 2, 2 };
//        Compute.median_mode(in);
//        assertTrue(Compute.median == 2);
//        assertTrue(Compute.mode == 2);
//    }

}