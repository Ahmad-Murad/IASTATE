package hw6;

public class MyWeighing implements Weighing<MyEdgeData> {

    @Override
    public double weight(MyEdgeData edge) {
        return edge.wt;
    }
}
