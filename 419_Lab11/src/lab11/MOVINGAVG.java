package lab11;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.pig.EvalFunc;
import org.apache.pig.data.DataBag;
import org.apache.pig.data.Tuple;
import org.apache.pig.data.TupleFactory;

public class MOVINGAVG extends EvalFunc<Tuple>
{
    // Start date of oct 1st 2013
    private static final long START_DATE = 20131001;

    @Override
    public Tuple exec(Tuple input) throws IOException {
        // Get as databag
        DataBag values = (DataBag) input.get(0);

        // Form all the tuples into StockData objects
        ArrayList<StockData> data = new ArrayList<>();
        for (Tuple tup : values) {
            StockData sd = new StockData((String) tup.get(0), (Long) tup.get(1), (Float) tup.get(2));
            data.add(sd);
        }

        // Sort the data by date
        Collections.sort(data);

        // Position i to the start of the desired date
        int i = 0;
        while (i < data.size() && data.get(i).date < START_DATE)
            i++;

        // Compute moving averages and append it into the big tuple
        TupleFactory tf = TupleFactory.getInstance();
        Tuple t = tf.newTuple();
        for (; i < data.size(); i++) {
            StockData cur = data.get(i);
            cur.movingAvg = getMovingAvg(data, i);
            t.append(cur.asTuple());
        }

        // Return the big tuple containing all the data for this particular ticker
        return t;
    }

    private float getMovingAvg(List<StockData> data, int targetIndex) {
        float tot = 0.0f;
        int numItems = 0;
        for (int i = targetIndex - 1; i >= 0 && numItems < 20; i--) {
            tot += data.get(i).openPrice;
            numItems++;
        }

        return tot / numItems;
    }

    private class StockData implements Comparable<StockData> {
        public final String ticker;
        public final long date;
        public final float openPrice;
        public float movingAvg;

        public StockData(String t, long d, float o) {
            ticker = t;
            date = d;
            openPrice = o;
        }

        @Override
        public int compareTo(StockData o) {
            return (int) (this.date - o.date);
        }

        public Tuple asTuple() {
            Tuple t = TupleFactory.getInstance().newTuple();
            t.append(ticker);
            t.append(date);
            t.append(openPrice);
            t.append(movingAvg);
            return t;
        }
    }
}
