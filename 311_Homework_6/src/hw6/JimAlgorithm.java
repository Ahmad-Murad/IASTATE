package hw6;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

public class JimAlgorithm<V, E> implements CoffeeSolver<V, E> {

    private class vWrapper {
        public final int id;
        public boolean marked = false;

        vWrapper(int id) {
            this.id = id;
        }
    }

    private ArrayList<Integer> sorted;
    private Set<vWrapper> unmarked;

    @Override
    public List<Integer> sortVertices(Graph<V, E> graph) {
        sorted = new ArrayList<Integer>();
        for (Integer i : graph.getVertices())
            unmarked.add(new vWrapper(i));

        while (!unmarked.isEmpty()) {
            dfs(unmarked.iterator().next());
        }

        return null;
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

    private void dfs(vWrapper vert) {}
}
