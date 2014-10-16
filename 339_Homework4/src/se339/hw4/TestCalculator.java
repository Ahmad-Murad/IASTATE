package se339.hw4;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestCalculator
{
    ExpressionContext ctx = new ExpressionContext();

    @Rule
    public TestName tn = new TestName();

    @Before
    public void beforeEach() {
        System.out.println("---> Starting " + tn.getMethodName());
    }

    @Test
    public void test01() {
        assertEquals(9, ctx.computeExpression("9"));
    }

    @Test
    public void test002() {
        assertEquals(99, ctx.computeExpression("99"));
    }

    @Test
    public void test03() {
        assertEquals(18, ctx.computeExpression("9+9"));
    }

    @Test
    public void test04() {
        assertEquals(27, ctx.computeExpression("9 + 10 + 10 - 2"));
    }

    @Test(expected = IllegalStateException.class)
    public void test05() {
        ctx.computeExpression(" - 9 + 10 + 10 - 2");
    }

    @Test(expected = IllegalStateException.class)
    public void test06() {
        ctx.computeExpression(" + 9 + 10 + 10 - 2");
    }

    @Test(expected = IllegalStateException.class)
    public void test07() {
        ctx.computeExpression("9 + 10 + 10 - 2 +");
    }

    @Test(expected = IllegalStateException.class)
    public void test08() {
        ctx.computeExpression("9 + 10 + 10 - 23 -");
    }

    @Test(expected = IllegalStateException.class)
    public void test09() {
        ctx.computeExpression("9 + 10 + + 10 - 23 -");
    }

    @Test
    public void test10() {
        assertEquals(-200, ctx.computeExpression("9 + 10 - 6 + 10 - 23 - 100 - 100"));
    }

    @Test
    public void test11() {
        assertEquals(9996, ctx.computeExpression("9999 + 10 + + 10 - 23"));
    }

    @Test
    public void test12() {
        assertEquals(6, ctx.computeExpression("9 + 10 + 0 + 10 - 23"));
    }

    @Test
    public void test13() {
        assertEquals(6, ctx.computeExpression("9 + 10 + + 10 - 23 - 0"));
    }

    @Test
    public void test14() {
        assertEquals(0, ctx.computeExpression("0 + 0 - 0 + 0"));
    }

    @Test(expected = IllegalStateException.class)
    public void test15() {
        ctx.computeExpression("9 * 10 + 10 - 23 -");
    }

    @Test(expected = IllegalStateException.class)
    public void test16() {
        ctx.computeExpression("* + 10 + 10 - 23 -");
    }

    @Test(expected = IllegalStateException.class)
    public void test17() {
        ctx.computeExpression("9 + 10 + 10 - 23 *");
    }

    @Test(expected = IllegalStateException.class)
    public void test18() {
        ctx.computeExpression("9 + 10 * * 10 - 23");
    }

    @Test(expected = IllegalStateException.class)
    public void test19() {
        ctx.computeExpression("9 + 10 * 10 - 23 &");
    }

    @Test(expected = IllegalStateException.class)
    public void test20() {
        ctx.computeExpression("9 + 10 * 10 - &");
    }

    @Test(expected = IllegalStateException.class)
    public void test21() {
        ctx.computeExpression("9 + 10.1 * 10");
    }
}
