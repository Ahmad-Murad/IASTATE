package Demo;

/**
 * @author Andrew
 */
public class PrinterI extends Demo._PrinterDisp {
    @Override
    public void printString(String s, Ice.Current current) {
        System.out.println(s);
    }
}
