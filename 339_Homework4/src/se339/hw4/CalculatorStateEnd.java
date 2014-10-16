/**
 * 
 */
package se339.hw4;

/**
 * @author Andrew
 * 
 */
public class CalculatorStateEnd extends CalculatorState
{
    @Override
    public void computeNext(ExpressionContext ctx, char cur) {
        ctx.total += (ctx.operator * ctx.curNumber);
        if (cur < '0' || cur > '9')
            throw new IllegalStateException("Encountered token " + cur + " at the end of the expression.");
    }

    @Override
    public void nextState(ExpressionContext ctx, char next) {
        throw new IllegalStateException("Cannot move to next state from ending state");
    }
}
