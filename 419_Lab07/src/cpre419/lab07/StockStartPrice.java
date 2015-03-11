package cpre419.lab07;

import java.io.IOException;

import org.apache.pig.EvalFunc;
import org.apache.pig.data.Tuple;

public class StockStartPrice extends EvalFunc<Tuple>
{

	@Override
	public Tuple exec(Tuple input) throws IOException {
		if (input == null || input.size() == 0 || input.get(0) == null)
			return null;
		return null;
	}

}
