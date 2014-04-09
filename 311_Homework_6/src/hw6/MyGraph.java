package hw6;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

public class MyGraph<V, E> implements Graph<V, E> {

    private HashMap<Integer, Vertex> vertices = new HashMap<Integer, Vertex>();
    private HashMap<Integer, Edge> edges = new HashMap<Integer, Edge>();

    private static int _eCount = 1;

    public class Edge
    {
        public int src;
        public int tar;
        public E attr;
        public final int id;

        Edge(int source, int target, E attribute) {
            this.src = source;
            this.tar = target;
            this.attr = attribute;
            this.id = _eCount++;
        }
    }

    private static int _vCount = 1;

    public class Vertex
    {
        public V vert;
        public Set<Integer> edges = new HashSet<Integer>();
        public final int id;

        Vertex(V node) {
            this.vert = node;
            this.id = _vCount++;
        }
    }

    @Override
    public int addVertex(V v) {

        // Create vertex and add to graph's vertices
        Vertex newV = new Vertex(v);
        vertices.put(newV.id, newV);
        return newV.id;
    }

    @Override
    public int addEdge(int srcID, int targetID, E attr)
                    throws IllegalArgumentException {

        // Validate input
        checkVertex(srcID);
        checkVertex(targetID);

        // Create new edge and add to edge set
        Edge newEdge = new Edge(srcID, targetID, attr);
        edges.put(newEdge.id, newEdge);

        // Give source vertex ref to new outgoing edge
        vertices.get(srcID).edges.add(newEdge.id);

        return newEdge.id;
    }

    @Override
    public Set<Integer> getVertices() {

        return vertices.keySet();
    }

    @Override
    public Set<Integer> getEdges() {

        return edges.keySet();
    }

    @Override
    public E getAttribute(int id) throws IllegalArgumentException {

        checkEdge(id);

        return edges.get(Integer.valueOf(id)).attr;
    }

    @Override
    public V getData(int id) throws IllegalArgumentException {

        checkVertex(id);

        return vertices.get(Integer.valueOf(id)).vert;
    }

    @Override
    public int getSource(int id) throws IllegalArgumentException {

        checkEdge(id);

        return edges.get(Integer.valueOf(id)).src;
    }

    @Override
    public int getTarget(int id) throws IllegalArgumentException {

        checkEdge(id);

        return edges.get(Integer.valueOf(id)).tar;
    }

    @Override
    public Set<Integer> getEdgesOf(int id) throws IllegalArgumentException {

        checkVertex(id);

        return vertices.get(Integer.valueOf(id)).edges;
    }

    private void checkEdge(int edgeId) throws IllegalArgumentException {
        if (!edges.containsKey(Integer.valueOf(edgeId)))
            throw new IllegalArgumentException("Edge id " + edgeId + " was not found in this graph.");
    }

    private void checkVertex(int vertexId) throws IllegalArgumentException {
        if (!vertices.containsKey(Integer.valueOf(vertexId)))
            throw new IllegalArgumentException("Vertex id " + vertexId + " was not found in this graph.");
    }
}
