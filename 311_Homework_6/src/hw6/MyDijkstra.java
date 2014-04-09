package hw6;

import java.util.HashMap;
import java.util.List;

public class MyDijkstra<V, E> implements Dijkstra<V, E> {

    private Weighing<E> weigher = null;
    private Graph<V, E> graph = null;
    private int start;
    private boolean mustCompute = true;
    private HashMap<Integer, V> vidToV = new HashMap<Integer, V>();

    private class ivVertex {
        double dist;
        V pred;
    }

    @Override
    public void setGraph(Graph<V, E> graph) {
        this.graph = graph;
    }

    @Override
    public void setStart(int startId) throws IllegalArgumentException,
                    IllegalStateException {

        if (graph == null)
            throw new IllegalStateException("Graph has not been set yet.");

        if (!graph.getVertices().contains(startId))
            throw new IllegalArgumentException("Vertex " + startId + " was not found in the graph.");

        start = startId;
        mustCompute = true;
    }

    @Override
    public void setWeighing(Weighing<E> weighing) {
        mustCompute = true;
        weigher = weighing;
    }

    @Override
    public void computeShortestPath() throws IllegalStateException {

        if (weigher == null || graph == null)
            throw new IllegalStateException("Weigher and graph must be set before calling computeShortestPath.");

        mustCompute = false;
        for (Integer vertID : graph.getVertices()) {

        }
    }

    @Override
    public List<Integer> getPath(int endId) throws IllegalArgumentException,
                    IllegalStateException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public double getCost(int endId) throws IllegalArgumentException,
                    IllegalStateException {
        // TODO Auto-generated method stub
        return 0;
    }
}
