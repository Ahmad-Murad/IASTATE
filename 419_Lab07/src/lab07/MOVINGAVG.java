package lab07;

import java.io.IOException;

import org.apache.pig.EvalFunc;
import org.apache.pig.data.DataBag;
import org.apache.pig.data.Tuple;

public class MOVINGAVG extends EvalFunc<Float>
{
	@Override
	public Float exec(Tuple input) throws IOException {
	    DataBag values = (DataBag)input.get(0);
	
	    float total = 0.0f;
	    int count = 0;
	    for(Tuple t : values){
            Float d = (Float) t.get(0);
            if (d == null) continue;
            total += d;
            count++;
            if(count >= 20)
            	break;
	    }
	
	    return total/count;
	}
}
