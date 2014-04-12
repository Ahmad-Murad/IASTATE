/**
 * 
 */
package hw6;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;

/**
 * @author aguibert
 * 
 */
public class MyJimSolver
{
    private static HashMap<Integer, Integer> fileToGraph = new HashMap<>();
    private static HashMap<Integer, Integer> graphToFile = new HashMap<>();

    /**
     * @param args
     */
    public static void main(String[] args) {

        JimAlgorithm<Integer, MyEdgeData> ja = new JimAlgorithm<>();

        // Parse file into a graph
        Graph<Integer, MyEdgeData> graph = parseFile();
        System.out.println(graph);

        List<Integer> ingList = getIngredientOrdering(graph);
        List<Integer> shortestPath = ja.shortestPath(graph, ingList, new MyWeighing());

        System.out.println("Shortest path is: " + shortestPath);
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
        List<Integer> sortedIngreds = new JimAlgorithm<Integer, MyEdgeData>().sortVertices(ingredGraph);
        System.out.println("Got ordering: " + sortedIngreds);
        List<Integer> corresponding = new ArrayList<>();
        for (int i : sortedIngreds)
            corresponding.add(ingreds[i]);
        System.out.println("Corresponding: " + corresponding);
        List<Integer> graphID = new ArrayList<>();
        for (int i : corresponding)
            graphID.add(fileToGraph.get(i));
        System.out.println("The ID's in the graph are: " + graphID);

        return graphID;
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
}
