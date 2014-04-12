package hw6;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class JimAlgorithm<V, E> implements CoffeeSolver<V, E> {

    private final int UNMARKED = 0;
    private final int TEMP_MARK = 1;
    private final int PERM_MARK = 2;

    private ArrayList<Integer> sorted;
    private Integer[] marks;
    private Graph<V, E> g;
    private int numUnmarked = 0, startSeed = 0;

    @Override
    public List<Integer> sortVertices(Graph<V, E> graph) {
        g = graph;
        sorted = new ArrayList<Integer>();
        marks = new Integer[graph.getVertices().size()];
        for (Integer i : graph.getVertices()) {
            marks[i] = UNMARKED;
            numUnmarked++;
        }

        Iterator<Integer> verts = graph.getVertices().iterator();
        for (int i = 0; i < startSeed; i++)
            verts.next();
        try {
            while (numUnmarked > 0) {
                Integer cur = (verts.hasNext()) ? verts.next() : (verts = graph.getVertices().iterator()).next();
                dfs(cur);
            }
        } catch (IllegalStateException ise) {
            return null; // There was a cycle in the graph
        }

        return sorted;
    }

    @Override
    public List<Integer> shortestPath(Graph<V, E> graph, List<Integer> locations, Weighing<E> weigh) {

        Dijkstra<V, E> dij = new MyDijkstra<>();
        dij.setGraph(graph);
        dij.setWeighing(weigh);
        List<Integer> shortestPath = new ArrayList<Integer>();
        for (int i = 0; i < locations.size() - 1; i++) {
            dij.setStart(locations.get(i)); // Set start to cur location
            dij.computeShortestPath();
            // Get path to next location, and add it to shortestPath result
            if (shortestPath.size() > 0)
                shortestPath.remove(shortestPath.size() - 1);
            shortestPath.addAll(dij.getPath(locations.get(i + 1)));
        }
        return shortestPath;
    }

    @Override
    public Collection<List<Integer>> generateValidSortS(Graph<V, E> graph) {
        // TODO not generating every type of sort
        Collection<List<Integer>> sorts = new ArrayList<List<Integer>>();
        startSeed = graph.getVertices().size();
        while (startSeed > 0) {
            List<Integer> sort = sortVertices(graph);
            if (!sorts.contains(sort))
                sorts.add(sort);
            startSeed--;
        }
        return sorts;
    }

    private void dfs(Integer vert) {
        // Check if graph has a cycle (not a DAG)
        if (marks[vert] == TEMP_MARK) {
            throw new IllegalStateException("Graph had a cycle in it.");
        }

        if (marks[vert] == UNMARKED) {
            mark(vert, TEMP_MARK); // mark vertex temporarily
            for (Integer curEdge : g.getEdgesOf(vert)) {
                dfs(g.getTarget(curEdge));
            }
            mark(vert, PERM_MARK);
            sorted.add(0, vert);
        }
    }

    private void mark(int toMark, int newMark) {
        int oldMark = marks[toMark];
        marks[toMark] = newMark;
        if (newMark == UNMARKED && oldMark != UNMARKED)
            numUnmarked++;
        else if (oldMark == UNMARKED && (newMark == TEMP_MARK || newMark == PERM_MARK))
            numUnmarked--;
    }
}
