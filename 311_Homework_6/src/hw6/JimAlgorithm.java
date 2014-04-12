package hw6;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.TreeSet;

public class JimAlgorithm<V, E> implements CoffeeSolver<V, E> {

    private final int UNMARKED = 0;
    private final int TEMP_MARK = 1;
    private final int PERM_MARK = 2;

    private ArrayList<Integer> sorted;
    private Integer[] marks;
    private Graph<V, E> g;
    private int numUnmarked = 0, startSeed = 0;

    private static HashMap<Integer, Integer> fileToGraph = new HashMap<>();
    private static HashMap<Integer, Integer> graphToFile = new HashMap<>();
    public static double mostRecentCost = 0.0; // For JUnit testing purposes

    public static void main(String... args) {

        String file = "files/newAmes.txt";
        if (args != null && args.length == 1)
            file = args[0];

        JimAlgorithm<Integer, MyEdgeData> ja = new JimAlgorithm<>();

        // Construct graph of graph file
        Graph<Integer, MyEdgeData> graph = parseFile(file);

        // Construct a DAG of the ingredient dependencies 
        List<Integer> ingList_graphIDs = getIngredientOrdering(graph);
        // Convert internal graph ID's to the ames.txt ID's
        List<Integer> ingList_fileIDs = new ArrayList<>();
        for (int i : ingList_graphIDs)
            ingList_fileIDs.add(graphToFile.get(i));
        System.out.println("Valid topological sorting: " + ingList_fileIDs);

        List<Integer> shortestPath_graphIDs = ja.shortestPath(graph, ingList_graphIDs, new MyWeighing());
        // Convert internal graph ID's to the ames.txt ID's
        List<Integer> shortestPath_fileIDs = new ArrayList<Integer>();
        for (int i : shortestPath_graphIDs)
            shortestPath_fileIDs.add(graphToFile.get(i));

        System.out.println("Path taken: " + shortestPath_fileIDs);
    }

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
        double cost = 0.0;
        for (int i = 0; i < locations.size() - 1; i++) {
            dij.setStart(locations.get(i)); // Set start to cur location
            dij.computeShortestPath();
            // Get path to next location, and add it to shortestPath result
            if (shortestPath.size() > 0)
                shortestPath.remove(shortestPath.size() - 1);
            shortestPath.addAll(dij.getPath(locations.get(i + 1)));
            cost += dij.getCost(locations.get(i + 1));
        }
        JimAlgorithm.mostRecentCost = cost;
        System.out.println("Cost for this path is: " + cost);

        return shortestPath;
    }

    @Override
    public Collection<List<Integer>> generateValidSortS(Graph<V, E> graph) {
        // TODO not generating every type of sort
        TreeSet<Integer> tree = new TreeSet<>();

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

    private static Graph<Integer, MyEdgeData> parseFile(String file) {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();

        try (Scanner s = new Scanner(new FileReader(file)))
        {
            // Parse verticies
            if (!"VERTICES:".equalsIgnoreCase(s.next()))
                throw new RuntimeException("First line of input file must indicate number of verticies.");
            int numVerticies = new Integer(s.nextInt());
            s.nextLine();
            for (int i = 0; i < numVerticies; i++) {
                String[] line = s.nextLine().split(",");
                int fileID = Integer.valueOf(line[0]);
                double lat = Double.valueOf(line[1]);
                double lon = Double.valueOf(line[2]);
                int graphID = g.addVertex(fileID);
                fileToGraph.put(fileID, graphID);
                graphToFile.put(graphID, fileID);
            }

            // Parse edges
            if (!"EDGES:".equalsIgnoreCase(s.next()))
                throw new RuntimeException("Expected number of edges to be given in input file.");
            int numEdges = new Integer(s.next());
            s.nextLine();
            for (int i = 0; i < numEdges; i++) {
                String[] line = s.nextLine().split(",");
                int src = Integer.valueOf(line[0]);
                int tar = Integer.valueOf(line[1]);
                double wt = Double.valueOf(line[2]);
                String street = (line.length == 4) ? line[3] : null;
                MyEdgeData data = new MyEdgeData(wt, street);
                g.addEdge(fileToGraph.get(src), fileToGraph.get(tar), data);
                g.addEdge(fileToGraph.get(tar), fileToGraph.get(src), data);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return g;
    }

    private static List<Integer> getIngredientOrdering(Graph<Integer, MyEdgeData> realGraph) {
        int[] ingreds = { 1055, 371, 2874, 2351, 2956, 1171, 1208, 2893 };
        Graph<Integer, MyEdgeData> ingredGraph = new MyGraph<>();
        for (int i : ingreds)
            ingredGraph.addVertex(i);
        ingredGraph.addEdge(0, 2, new MyEdgeData(0.0, "A->C"));
        ingredGraph.addEdge(0, 5, new MyEdgeData(0.0, "A->F"));
        ingredGraph.addEdge(1, 2, new MyEdgeData(0.0, "B->C"));
        ingredGraph.addEdge(1, 3, new MyEdgeData(0.0, "B->D"));
        ingredGraph.addEdge(2, 3, new MyEdgeData(0.0, "C->D"));
        ingredGraph.addEdge(2, 4, new MyEdgeData(0.0, "C->E"));
        ingredGraph.addEdge(5, 2, new MyEdgeData(0.0, "F->C"));
        ingredGraph.addEdge(5, 4, new MyEdgeData(0.0, "F->E"));
        ingredGraph.addEdge(0, 6, new MyEdgeData(0.0, "A->Jim"));
        ingredGraph.addEdge(1, 6, new MyEdgeData(0.0, "B->Jim"));
        ingredGraph.addEdge(2, 6, new MyEdgeData(0.0, "C->Jim"));
        ingredGraph.addEdge(3, 6, new MyEdgeData(0.0, "D->Jim"));
        ingredGraph.addEdge(4, 6, new MyEdgeData(0.0, "E->Jim"));
        ingredGraph.addEdge(5, 6, new MyEdgeData(0.0, "F->Jim"));
        // We always have to be the starting point
        ingredGraph.addEdge(7, 0, new MyEdgeData(0.0, "A->You"));
        ingredGraph.addEdge(7, 1, new MyEdgeData(0.0, "B->You"));
        ingredGraph.addEdge(7, 2, new MyEdgeData(0.0, "C->You"));
        ingredGraph.addEdge(7, 3, new MyEdgeData(0.0, "D->You"));
        ingredGraph.addEdge(7, 4, new MyEdgeData(0.0, "E->You"));
        ingredGraph.addEdge(7, 5, new MyEdgeData(0.0, "F->You"));
        ingredGraph.addEdge(7, 6, new MyEdgeData(0.0, "G->You"));
        List<Integer> sortedIngreds = new JimAlgorithm<Integer, MyEdgeData>().sortVertices(ingredGraph);
        List<Integer> corresponding = new ArrayList<>();
        for (int i : sortedIngreds)
            corresponding.add(ingreds[i]);
        List<Integer> graphID = new ArrayList<>();
        for (int i : corresponding) {
            graphID.add(fileToGraph.get(i));
        }

        return graphID;
    }
}
