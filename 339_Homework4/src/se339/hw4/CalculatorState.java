package se339.hw4;

public abstract class CalculatorState {
    abstract void computeNext(ExpressionContext ctx, char cur);

    public void nextState(ExpressionContext ctx, char next) {
        if (next >= '0' && next <= '9') {
            if (!(ctx.state instanceof CalculatorStateNumber))
                ctx.state = new CalculatorStateNumber();
        } else if (next == '+' || next == '-') {
            if (!(ctx.state instanceof CalculatorStateOperator))
                ctx.state = new CalculatorStateOperator();
        } else if (next == '$') {
            if (!(ctx.state instanceof CalculatorStateEnd))
                ctx.state = new CalculatorStateEnd();
        } else
            throw new IllegalStateException("Encountered token " + next);
    }
}
