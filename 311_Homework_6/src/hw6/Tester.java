package hw6;

import static org.junit.Assert.fail;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

import org.junit.BeforeClass;
import org.junit.Test;

public class Tester {

    static Graph<String, String> ingredients = new MyGraph<String, String>();
    static String[] verticies = { "A", "B", "C", "D", "E", "F", "G" };
    private static HashMap<Integer, Integer> fileToGraph = new HashMap<>();
    private static HashMap<Integer, Integer> graphToFile = new HashMap<>();

    @BeforeClass
    public static void beforeAll() {
        for (String vert : verticies)
            ingredients.addVertex(vert);

        ingredients.addEdge(0, 2, "A->C");
        ingredients.addEdge(0, 5, "A->F");
        ingredients.addEdge(1, 2, "B->C");
        ingredients.addEdge(1, 3, "B->D");
        ingredients.addEdge(2, 3, "C->D");
        ingredients.addEdge(2, 4, "C->E");
        ingredients.addEdge(5, 2, "F->C");
        ingredients.addEdge(5, 4, "F->E");
        ingredients.addEdge(0, 6, "A->G");
        ingredients.addEdge(1, 6, "B->G");
        ingredients.addEdge(2, 6, "C->G");
        ingredients.addEdge(3, 6, "D->G");
        ingredients.addEdge(4, 6, "E->G");
        ingredients.addEdge(5, 6, "F->G");
    }

    @Test
    public void testBasic() {
        if (ingredients.getVertices().size() != verticies.length)
            fail("Graph did not contain " + verticies.length + " verticies.");

        for (int i = 0; i < verticies.length; i++)
            for (Integer curEdge : ingredients.getEdgesOf(i))
                System.out.println(verticies[i] + " -> " + verticies[ingredients.getTarget(curEdge)]);
    }

    @Test
    public void testTopSort() {
        JimAlgorithm<String, String> ja = new JimAlgorithm<String, String>();
        List<Integer> aSort = ja.sortVertices(ingredients);
        System.out.println("Topological sort: " + getOrdering(ingredients, aSort));
        if (aSort.size() != verticies.length)
            fail("Expected " + verticies.length + " verticies.  Instead got " + aSort.size());
    }

    //@Test
    public void testAllTopSorts() {
        JimAlgorithm<String, String> ja = new JimAlgorithm<String, String>();
        Collection<List<Integer>> allSorts = ja.generateValidSortS(ingredients);
        for (List<Integer> aSort : allSorts) {
            System.out.println("Potential top. sort: " + aSort);
            if (aSort.size() != verticies.length)
                fail("Expected " + verticies.length + " verticies.  Instead got " + aSort.size());
        }
    }

    @Test
    public void testJim() {
        // Map from ames.txt ID -> graph ID
        HashMap<Integer, Integer> vs = new HashMap<Integer, Integer>();
        Graph<Integer, MyEdgeData> g = parseFile();

        //                A     B    C     D     E     F     Jim   Me
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
        List<Integer> locs = new JimAlgorithm<String, String>().sortVertices(ingredients);
        List<Integer> realLocs = new ArrayList<Integer>();
        for (int i : locs)
            realLocs.add(ingreds[i]);
        System.out.println(" Got sorting of: " + realLocs);
        JimAlgorithm<Integer, MyEdgeData> jim = new JimAlgorithm<>();
        Weighing<MyEdgeData> weigh = new MyWeighing();
        List<Integer> path = jim.shortestPath(g, realLocs, weigh);
        System.out.println("GOT FINAL JIM PATH: " + path);
    }

    @Test
    public void testShortestPathSimple1() {
        JimAlgorithm<Integer, MyEdgeData> jim = new JimAlgorithm<>();
        Weighing<MyEdgeData> weigh = new MyWeighing();
        List<Integer> locs = new ArrayList<Integer>(Arrays.asList(0, 2, 5));
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        for (int i = 0; i < 6; i++)
            g.addVertex(i);
        g.addEdge(0, 1, new MyEdgeData(1.0, null));
        g.addEdge(1, 2, new MyEdgeData(1.0, null));
        g.addEdge(2, 3, new MyEdgeData(2.0, null));
        g.addEdge(3, 4, new MyEdgeData(2.0, null));
        g.addEdge(4, 5, new MyEdgeData(1.0, null));
        List<Integer> path = jim.shortestPath(g, locs, weigh);
        System.out.println("Shortest path jim1: " + path);
    }

    @Test
    public void testShortestPathSimple2() {
        JimAlgorithm<Integer, MyEdgeData> jim = new JimAlgorithm<>();
        Weighing<MyEdgeData> weigh = new MyWeighing();
        List<Integer> locs = new ArrayList<Integer>(Arrays.asList(0, 2, 5));
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        for (int i = 0; i < 6; i++)
            g.addVertex(i);
        g.addEdge(0, 1, new MyEdgeData(1.0, null));
        g.addEdge(1, 2, new MyEdgeData(1.0, null));
        g.addEdge(2, 3, new MyEdgeData(1.0, null));
        g.addEdge(0, 2, new MyEdgeData(50.0, null));
        g.addEdge(3, 4, new MyEdgeData(2.0, null));
        g.addEdge(2, 5, new MyEdgeData(50.0, null));
        List<Integer> path = jim.shortestPath(g, locs, weigh);
        System.out.println("Shortest path jim2: " + path);
    }

    @Test
    public void testShortestPath1() {
        JimAlgorithm<Integer, MyEdgeData> jim = new JimAlgorithm<>();
        Weighing<MyEdgeData> weigh = new MyWeighing();
        List<Integer> locs = new ArrayList<Integer>(Arrays.asList(0, 2, 5));
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        for (int i = 0; i < 6; i++)
            g.addVertex(i);
        g.addEdge(0, 1, new MyEdgeData(1.0, null));
        g.addEdge(1, 2, new MyEdgeData(1.0, null));
        g.addEdge(2, 3, new MyEdgeData(1.0, null));
        g.addEdge(0, 2, new MyEdgeData(50.0, null));
        g.addEdge(3, 4, new MyEdgeData(2.0, null));
        g.addEdge(2, 5, new MyEdgeData(50.0, null));
        List<Integer> path = jim.shortestPath(g, locs, weigh);
        System.out.println("Shortest path jim3: " + path);
    }

    @Test
    public void testCycleGraph() {
        Graph<String, String> cycle = new MyGraph<String, String>();
        cycle.addVertex("A");
        cycle.addVertex("B");
        cycle.addEdge(0, 1, "A->B");
        cycle.addEdge(1, 0, "B->A");
        JimAlgorithm<String, String> ja = new JimAlgorithm<>();
        List<Integer> sort = ja.sortVertices(cycle);
        if (sort != null)
            fail("Should have been a cycle in the graph.");
    }

    @Test
    public void testDisconnected() {
        Graph<String, String> dc = new MyGraph<String, String>();
        dc.addVertex("A");
        dc.addVertex("B");
        dc.addVertex("C");
        JimAlgorithm<String, String> ja = new JimAlgorithm<>();
        List<Integer> sort = ja.sortVertices(dc);
        System.out.println("Disconnected graph: " + getOrdering(dc, sort));
    }

    // TODO @Test
    public void testAllSorts() {
        Graph<String, String> dc = new MyGraph<String, String>();
        dc.addVertex("A");
        dc.addVertex("B");
        dc.addVertex("C");
        dc.addEdge(0, 1, "0->1");
        JimAlgorithm<String, String> ja = new JimAlgorithm<>();
        Collection<List<Integer>> sorts = ja.generateValidSortS(dc);
        for (List<Integer> sort : sorts)
            System.out.println("Disconnected potential sort: " + getOrdering(dc, sort));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testInvalidStart() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(0);

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(1);
    }

    @Test(expected = IllegalStateException.class)
    public void testNotSetGraph() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setStart(1);
    }

    @Test(expected = IllegalStateException.class)
    public void testInvalidCompute1() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(new MyGraph<Integer, MyEdgeData>());
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
    }

    @Test(expected = IllegalStateException.class)
    public void testInvalidCompute2() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        Graph<Integer, MyEdgeData> g = new MyGraph<Integer, MyEdgeData>();
        g.addVertex(0);
        myDij.setGraph(g);
        myDij.setStart(0);
        myDij.computeShortestPath();
    }

    @Test(expected = IllegalStateException.class)
    public void testInvalidCompute3() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        Graph<Integer, MyEdgeData> g = new MyGraph<Integer, MyEdgeData>();
        g.addVertex(0);
        myDij.setGraph(g);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
    }

    @Test
    public void testDijkstra1() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(0);
        g.addVertex(1);
        g.addVertex(2);
        g.addVertex(3);
        g.addEdge(0, 1, new MyEdgeData(1.0, "Street 1"));
        g.addEdge(1, 2, new MyEdgeData(1.0, "Street 2"));
        g.addEdge(0, 3, new MyEdgeData(0.1, "quick1"));
        g.addEdge(3, 2, new MyEdgeData(0.1, "quick2"));

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(0);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
        List<Integer> path = myDij.getPath(2);
        System.out.println("Test1 dijkstra: " + path);
    }

    @Test
    public void testDijkstra2() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(0);
        g.addVertex(1);
        g.addVertex(2);
        g.addVertex(3);
        g.addEdge(0, 1, new MyEdgeData(1.0, "Street 1"));
        g.addEdge(1, 2, new MyEdgeData(1.0, "Street 2"));
        g.addEdge(0, 3, new MyEdgeData(0.1, "quick1"));
        g.addEdge(3, 2, new MyEdgeData(0.1, "quick2"));

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(0);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
        double cost = myDij.getCost(2);
        if (cost != 0.2)
            fail("Expected cost to be 0.2 but was: " + cost);
    }

    private static Graph<Integer, MyEdgeData> parseFile() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();

        try (Scanner s = new Scanner(new FileReader("files/newAmes.txt")))
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

    private String getOrdering(Graph<String, String> graph, List<Integer> sort)
    {
        StringBuffer sb = new StringBuffer();
        for (Integer i : sort) {
            sb.append(graph.getData(i));
            sb.append(' ');
        }
        return sb.toString();
    }
}
