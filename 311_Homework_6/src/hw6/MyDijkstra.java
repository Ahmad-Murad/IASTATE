package hw6;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class MyDijkstra<V, E> implements Dijkstra<V, E> {

    private Weighing<E> weigher = null;
    private Graph<V, E> graph = null;
    private int start = -1;
    private boolean mustCompute = true;
    private HashMap<Integer, Double> dist = null;
    private HashMap<Integer, Integer> prev = null;

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
        if (start == -1)
            throw new IllegalStateException("Must set starting point before computing shortest path.");

        mustCompute = false;

        dist = new HashMap<Integer, Double>();
        prev = new HashMap<Integer, Integer>();

        // Do Dijkstra's
        Set<Integer> verticies = new HashSet<Integer>();
        for (Integer vert : graph.getVertices()) {
            dist.put(vert, Double.MAX_VALUE);
            prev.put(vert, null);
            verticies.add(vert); // need to make a deep copy of graph verticies
        }

        dist.put(start, 0.0);
        while (!verticies.isEmpty()) {
            // Compute vertex with shortest distance
            Integer shortest = null;
            Double shortestDist = Double.MAX_VALUE;
            for (Integer vert : dist.keySet()) {
                if (verticies.contains(vert) && dist.get(vert) < shortestDist) {
                    shortestDist = dist.get(vert);
                    shortest = vert;
                }
            }
            // Remove this node from the set
            verticies.remove(shortest);
            // If vertex is uninitialized, discontinue this iteration
            if (dist.get(shortest) == Double.MAX_VALUE)
                break;
            // Find the next closest neighbor
            for (Integer neighborEdge : graph.getEdgesOf(shortest)) {
                Integer neighbor = graph.getTarget(neighborEdge);
                Double pathLen = weigher.weight(graph.getAttribute(neighborEdge)) + dist.get(shortest);
                if (pathLen < dist.get(neighbor)) {
                    dist.put(neighbor, pathLen);
                    prev.put(neighbor, shortest);
                }
            }
        }
    }

    @Override
    public List<Integer> getPath(int endId) throws IllegalArgumentException,
                    IllegalStateException {

        if (mustCompute)
            throw new IllegalStateException("Need to set graph and start point before getting a path.");
        if (!graph.getVertices().contains(endId))
            throw new IllegalArgumentException("Did not find " + endId + " in the current graph.");

        List<Integer> path = new ArrayList<Integer>();
        Integer previous = prev.get(endId);
        path.add(0, endId);
        // While previous is defined
        while (previous != null && previous != Double.MAX_VALUE) {
            path.add(0, previous); // Build path, inserting at the front as we go
            previous = prev.get(previous);
        }
        if (path.get(0) != start)
            throw new RuntimeException("Path did not lead back to starting node (" + start + ").  Only got to " + path.get(0));

        return path;
    }

    @Override
    public double getCost(int endId) throws IllegalArgumentException,
                    IllegalStateException {
        if (mustCompute)
            throw new IllegalStateException("Need to set graph and start point before getting a path.");
        if (!graph.getVertices().contains(endId))
            throw new IllegalArgumentException("Did not find " + endId + " in the current graph.");

        return dist.get(endId);
    }
}
