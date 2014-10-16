package se339.hw4;

public class ExpressionContext {
    public static char END = '$';

    public CalculatorState state = new CalculatorStateNumber();
    public int total = 0;
    public int operator = 1;
    public int curNumber = 0;

    public ExpressionContext() {}

    public int computeExpression(String expr) throws IllegalStateException {
        if (expr == null)
            throw new IllegalStateException("Nothing to compute");

        String trimmed = expr.replace(" ", "").replace("" + END, "%");
        trimmed += END; // Append an ending token to the expression

        for (int i = 0; i < trimmed.length() - 1; i++) {
            state.computeNext(this, trimmed.charAt(i));
            state.nextState(this, trimmed.charAt(i + 1));
        }
        state.computeNext(this, trimmed.charAt(trimmed.length() - 2)); // do any final state logic on the last state

        return total;
    }
}
