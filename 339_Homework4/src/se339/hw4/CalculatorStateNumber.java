package se339.hw4;

public class CalculatorStateNumber extends CalculatorState
{
    @Override
    public void computeNext(ExpressionContext ctx, char cur)
    {
        if (!(cur >= '0' && cur <= '9'))
            throw new IllegalStateException("Encountered token " + cur);

        ctx.curNumber *= 10;
        ctx.curNumber += (cur - '0');
    }
}
