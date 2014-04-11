/**
 * 
 */
package hw6;

import java.io.FileReader;
import java.io.IOException;
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
        List<Integer> sortedIngredients = ja.sortVertices(graph);
//        ja.shortestPath(graph, locations, weigh)
    }

    private static Graph<Integer, MyEdgeData> parseFile() {
        Graph<Integer, MyEdgeData> g = new MyGraph<>();

        try (Scanner s = new Scanner(new FileReader("files/ames.txt")))
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
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return g;
    }
}
