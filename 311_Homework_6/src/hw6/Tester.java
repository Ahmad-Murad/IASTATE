package hw6;

import static org.junit.Assert.fail;

import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

import org.junit.BeforeClass;
import org.junit.Test;

public class Tester {

    static Graph<String, String> graph = new MyGraph<String, String>();
    static String[] verticies = { "A", "B", "C", "D", "E", "F", "G" };

    @BeforeClass
    public static void beforeAll() {
        for (String vert : verticies)
            graph.addVertex(vert);

        graph.addEdge(1, 3, "A->C");
        graph.addEdge(1, 6, "A->F");
        graph.addEdge(2, 3, "B->C");
        graph.addEdge(2, 4, "B->D");
        graph.addEdge(3, 4, "C->D");
        graph.addEdge(3, 5, "C->E");
        graph.addEdge(6, 3, "F->C");
        graph.addEdge(6, 5, "F->E");
        graph.addEdge(1, 7, "A->G");
        graph.addEdge(2, 7, "B->G");
        graph.addEdge(3, 7, "C->G");
        graph.addEdge(4, 7, "D->G");
        graph.addEdge(5, 7, "E->G");
        graph.addEdge(6, 7, "F->G");
    }

    @Test
    public void testBasic() {
        if (graph.getVertices().size() != verticies.length)
            fail("Graph did not contain " + verticies.length + " verticies.");

        for (int i = 1; i <= verticies.length; i++)
            for (Integer curEdge : graph.getEdgesOf(i))
                System.out.println(verticies[i - 1] + " -> " + verticies[graph.getTarget(curEdge) - 1]);
    }

    @Test
    public void testTopSort() {
        JimAlgorithm<String, String> ja = new JimAlgorithm<String, String>();
        List<Integer> aSort = ja.sortVertices(graph);
        System.out.println("Topological sort: " + getOrdering(graph, aSort));
        if (aSort.size() != verticies.length)
            fail("Expected " + verticies.length + " verticies.  Instead got " + aSort.size());
    }

    @Test
    public void testAllTopSorts() {
        JimAlgorithm<String, String> ja = new JimAlgorithm<String, String>();
        Collection<List<Integer>> allSorts = ja.generateValidSortS(graph);
        for (List<Integer> aSort : allSorts) {
            System.out.println("Potential top. sort: " + aSort);
            if (aSort.size() != verticies.length)
                fail("Expected " + verticies.length + " verticies.  Instead got " + aSort.size());
        }
    }

    //@Test
    public void testJim() {
        // Map from ames.txt ID -> graph ID
        HashMap<Integer, Integer> vs = new HashMap<Integer, Integer>();
        Graph<String, MyEdgeData> g = parseFile(vs);
        if (g.getEdgesOf(vs.get(2893)).size() != 1)
            fail("Size was: " + g.getEdgesOf(2893).size());
        for (Integer edgeID : g.getEdgesOf(vs.get(2893))) {
            if (!String.valueOf(g.getAttribute(edgeID).wt).contains("92.919"))
                fail("Weight was: " + g.getAttribute(edgeID).wt);
        }
    }

    @Test
    public void testCycleGraph() {
        Graph<String, String> cycle = new MyGraph<String, String>();
        cycle.addVertex("A");
        cycle.addVertex("B");
        cycle.addEdge(1, 2, "A->B");
        cycle.addEdge(2, 1, "B->A");
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

    @Test
    public void testAllSorts() {
        Graph<String, String> dc = new MyGraph<String, String>();
        dc.addVertex("A");
        dc.addVertex("B");
        dc.addVertex("C");
        dc.addEdge(1, 2, "1->2");
        JimAlgorithm<String, String> ja = new JimAlgorithm<>();
        Collection<List<Integer>> sorts = ja.generateValidSortS(dc);
        for (List<Integer> sort : sorts)
            System.out.println("Disconnected potential sort: " + getOrdering(dc, sort));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testInvalidStart() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(1);

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(2);
    }

    @Test(expected = IllegalStateException.class)
    public void testNotSetGraph() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setStart(2);
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
        g.addVertex(1);
        myDij.setGraph(g);
        myDij.setStart(1);
        myDij.computeShortestPath();
    }

    @Test(expected = IllegalStateException.class)
    public void testInvalidCompute3() {
        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        Graph<Integer, MyEdgeData> g = new MyGraph<Integer, MyEdgeData>();
        g.addVertex(1);
        myDij.setGraph(g);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
    }

    @Test
    public void testDijkstra1() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(1);
        g.addVertex(2);
        g.addVertex(3);
        g.addVertex(4);
        g.addEdge(1, 2, new MyEdgeData(1.0, "Street 1"));
        g.addEdge(2, 3, new MyEdgeData(1.0, "Street 2"));
        g.addEdge(1, 4, new MyEdgeData(0.1, "quick1"));
        g.addEdge(4, 3, new MyEdgeData(0.1, "quick2"));

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(1);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
        List<Integer> path = myDij.getPath(3);
        System.out.println("Test1 dijkstra: " + path);
    }

    @Test
    public void testDijkstra2() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();
        g.addVertex(1);
        g.addVertex(2);
        g.addVertex(3);
        g.addVertex(4);
        g.addEdge(1, 2, new MyEdgeData(1.0, "Street 1"));
        g.addEdge(2, 3, new MyEdgeData(1.0, "Street 2"));
        g.addEdge(1, 4, new MyEdgeData(0.1, "quick1"));
        g.addEdge(4, 3, new MyEdgeData(0.1, "quick2"));

        Dijkstra<Integer, MyEdgeData> myDij = new MyDijkstra<>();
        myDij.setGraph(g);
        myDij.setStart(1);
        myDij.setWeighing(new MyWeighing());
        myDij.computeShortestPath();
        double cost = myDij.getCost(3);
        if (cost != 0.2)
            fail("Expected cost to be 0.2 but was: " + cost);
    }

    private Graph<String, MyEdgeData> parseFile(HashMap<Integer, Integer> verts) {
        Graph<String, MyEdgeData> g = new MyGraph<String, MyEdgeData>();

        try (Scanner s = new Scanner(new FileReader("files/ames.txt")))
        {
            // Parse verticies
            if (!"VERTICES:".equalsIgnoreCase(s.next()))
                throw new RuntimeException("First line of input file must indicate number of verticies.");
            int numVerticies = new Integer(s.nextInt());
            s.nextLine();
            for (int i = 0; i < numVerticies; i++) {
                String[] line = s.nextLine().split(",");
                int vID = Integer.valueOf(line[0]);
                double lat = Double.valueOf(line[1]);
                double lon = Double.valueOf(line[2]);
                String data = "ID:" + vID + " LAT:" + lat + " LON:" + lon;
                verts.put(vID, g.addVertex(data));
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
                g.addEdge(verts.get(src), verts.get(tar), data);
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
