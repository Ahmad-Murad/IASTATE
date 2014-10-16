package se339.hw4;

public class CalculatorStateOperator extends CalculatorState
{
    @Override
    public void computeNext(ExpressionContext ctx, char cur)
    {
        ctx.total += (ctx.operator * ctx.curNumber);
        ctx.curNumber = 0;

        if (cur == '+')
            ctx.operator = 1;
        else if (cur == '-')
            ctx.operator = -1;
        else
            throw new IllegalStateException("Encountered token " + cur);
    }
}
