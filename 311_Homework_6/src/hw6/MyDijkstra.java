package hw6;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class MyDijkstra<V, E> implements Dijkstra<V, E> {

    private Weighing<E> weigher = null;
    private Graph<V, E> graph = null;
    private int start = -1;
    private boolean mustCompute = true;
    private HashMap<Integer, V> vidToV = new HashMap<Integer, V>();
    private Double dist[] = null;
    private ArrayList<V> prev = null;

    private class ivVertex {
        double dist;
        V pred;
    }

    @Override
    public void setGraph(Graph<V, E> graph) {
        this.graph = graph;
        int size = graph.getVertices().size() + 1;
        dist = new Double[size];
        prev = new ArrayList<V>(size);
        prev.set(0, null);
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
        if (start == -1)
            throw new IllegalStateException("Must set starting point before computing shortest path.");

        mustCompute = false;

        // Do Dijkstra's
        for (Integer vert : graph.getVertices()) {
            dist[vert] = Double.MAX_VALUE;
            prev.set(vert, null);
        }

        Set<Integer> verticies = graph.getVertices();
        dist[start] = 0.0; // TODO LEFTOFF implementing dijikstra's from wikipedia
    }

    @Override
    public List<Integer> getPath(int endId) throws IllegalArgumentException,
                    IllegalStateException {

        if (mustCompute)
            throw new IllegalStateException("Need to set graph and start point before getting a path.");
        if (!graph.getVertices().contains(endId))
            throw new IllegalArgumentException("Did not find " + endId + " in the current graph.");
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
