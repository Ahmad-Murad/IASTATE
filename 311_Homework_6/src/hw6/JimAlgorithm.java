package hw6;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class JimAlgorithm<V, E> implements CoffeeSolver<V, E> {

    private final int UNMARKED = 0;
    private final int TEMP_MARK = 1;
    private final int PERM_MARK = 2;

    private ArrayList<Integer> sorted;
    private Integer[] marks;
    private Graph<V, E> g;
    private int numUnmarked = 0;

    @Override
    public List<Integer> sortVertices(Graph<V, E> graph) {
        g = graph;
        sorted = new ArrayList<Integer>();
        marks = new Integer[graph.getVertices().size() + 1];
        for (Integer i : graph.getVertices()) {
            marks[i] = UNMARKED;
            numUnmarked++;
        }

        int i = 1;
        while (numUnmarked > 0) {
            dfs(i++);
        }

        return sorted;
    }

    @Override
    public List<Integer> shortestPath(Graph<V, E> graph,
                                      List<Integer> locations, Weighing<E> weigh) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Collection<List<Integer>> generateValidSortS(Graph<V, E> graph) {
        return null;
    }

    private void dfs(Integer vert) {
        // Check if graph has a cycle (not a DAG)
        if (marks[vert] == TEMP_MARK) {
            numUnmarked = 0;
            sorted = null;
            return;
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
