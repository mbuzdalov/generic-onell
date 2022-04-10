package ru.ifmo.onell.problem.mst.util;

import java.util.*;

/**
 * Implementation of dynamic connectivity based on a hierarchy of partial forests.
 *
 * Source: <a href="https://github.com/Aksenov239/concurrent-graph/blob/master/src/sequential/SequentialDynamicGraph.java">Aksenov239/concurrent-graph</a>.
 *
 * @author Vitaly Aksenov
 */
public final class AksenovDynamicGraph {
    private static final boolean CRAZY_ASSERTIONS = false;

    private final Random rnd = new Random(239);

    private static class Edge {
        int u, v, level;

        public Edge(int u, int v) {
            assert u != v;
            this.u = u;
            this.v = v;
            level = 0;
        }

        public int hashCode() {
            return u * 1_000_000 + v;
        }

        public boolean equals(Object o) {
            if (o instanceof Edge) {
                Edge e = (Edge) o;
                return e.u == u && e.v == v;
            }
            return false;
        }

        public String toString() {
            return u + " " + v;
        }
    }

    private class Node {
        Node l, r, p;
        final int y;
        int size;
        final Edge myEdge;
        final int id;
        final int level;
        boolean hasVertex;
        boolean hasEdge;

        public Node(int id, int level) {
            y = rnd.nextInt();
            size = 1;
            this.id = id;
            this.level = level;
            this.myEdge = null;
            hasVertex = isHasVertex();
            hasEdge = isHasEdge();
        }

        public Node(Edge edge, int id, int level) {
            y = rnd.nextInt();
            size = 1;
            this.id = id;
            this.level = level;
            this.myEdge = edge;
            hasVertex = isHasVertex();
            hasEdge = isHasEdge();
        }

        public void update() {
            size = getSizeNode(l) + getSizeNode(r) + 1;
            hasVertex = getHasVertexNode(l) || getHasVertexNode(r) || isHasVertex();
            hasEdge = getHasEdgeNode(l) || getHasEdgeNode(r) || isHasEdge();
            if (l != null) {
                l.p = this;
            }
            if (r != null) {
                r.p = this;
            }
        }

        public boolean isHasVertex() {
            if (myEdge == null) {
                return !adjacent[id][level].isEmpty();
            }
            return false;
        }

        public boolean isHasEdge() {
            if (myEdge != null) {
                return myEdge.level == level;
            }
            return false;
        }

        public String toString() {
            String me;
            if (myEdge != null) {
                me = myEdge.u + "->" + myEdge.v;
            } else {
                me = String.valueOf(id);
            }

            return "[" + (l == null ? "" : l + ",") + me + (r == null ? "" : "," + r) + "]";
        }
    }

    private static int getSizeNode(Node node) {
        return node == null ? 0 : node.size;
    }

    private static boolean getHasVertexNode(Node node) {
        return node != null && node.hasVertex;
    }

    private static boolean getHasEdgeNode(Node node) {
        return node != null && node.hasEdge;
    }

    private static Node merge(Node l, Node r) {
        assert l != r;

        if (l == null) {
            return r;
        }
        if (r == null) {
            return l;
        }
        if (l.y > r.y) {
            l.r = merge(l.r, r);
            l.update();
            return l;
        } else {
            r.l = merge(l, r.l);
            r.update();
            return r;
        }
    }

    private static void split(Node v, int size, Node[] answer) {
        if (v == null) {
            answer[0] = null;
            answer[1] = null;
        } else if (getSizeNode(v.l) >= size) {
            split(v.l, size, answer);
            v.l = answer[1];
            v.update();
            v.p = null;
            answer[1] = v;
        } else {
            split(v.r, size - getSizeNode(v.l) - 1, answer);
            v.r = answer[0];
            v.update();
            v.p = null;
            answer[0] = v;
        }
    }

    private static Node getRoot(Node v) {
        while (v.p != null) {
            v = v.p;
        }
        return v;
    }

    private static int getPosition(Node v) {
        int sum = getSizeNode(v.l);
        while (v.p != null) {
            if (v.p.r == v) {
                sum += getSizeNode(v.p.l) + 1;
            }
            v = v.p;
        }
        return sum;
    }

    private static class NodePair {
        public final Node a, b;
        public NodePair(Node a, Node b) {
            this.a = a;
            this.b = b;
        }
    }

    private class Forest {
        int level;
        Node[] vertexNode;
        HashMap<Edge, NodePair> nodeByEdge;
        Node[] splitTemp = new Node[2];

        public Forest(int n, int level) {
            this.level = level;
            nodeByEdge = new HashMap<>();
            vertexNode = new Node[n];
            for (int i = 0; i < n; i++) {
                vertexNode[i] = new Node(i, level);
            }
        }

        public void updateToTop(Node v) {
            while (v != null) {
                v.update();
                v = v.p;
            }
        }

        public void makeFirst(Node v) {
            Node head = getRoot(v);
            int pos = getPosition(v);
            split(head, pos, splitTemp);
            merge(splitTemp[1], splitTemp[0]);
        }

        public void link(Edge e) {
            int u = e.u;
            int v = e.v;

            makeFirst(vertexNode[u]);
            makeFirst(vertexNode[v]);
            Node n1 = getRoot(vertexNode[u]);
            Node n2 = getRoot(vertexNode[v]);

            int edgeId = edgeIndex.get(e);
            Node c1 = new Node(e, edgeId, level);
            Node c2 = new Node(e, edgeId, level);
            nodeByEdge.put(e, new NodePair(c1, c2));

            merge(merge(merge(n1, c1), n2), c2);
        }

        public void cut(Edge e) {
            int u = e.u;
            makeFirst(vertexNode[u]);
            NodePair c = nodeByEdge.get(e);
            nodeByEdge.remove(e);

            int pos1 = getPosition(c.a);
            int pos2 = getPosition(c.b);

            if (pos1 > pos2) {
                int q = pos1;
                pos1 = pos2;
                pos2 = q;
            }
            Node head = getRoot(vertexNode[u]);

            split(head, pos2 + 1, splitTemp);
            Node t11 = splitTemp[1];
            split(splitTemp[0], pos2, splitTemp);
            assert splitTemp[1] == c.a || splitTemp[1] == c.b;
            split(splitTemp[0], pos1 + 1, splitTemp);
            split(splitTemp[0], pos1, splitTemp);
            assert splitTemp[1] == c.a || splitTemp[1] == c.b;
            merge(splitTemp[0], t11);
        }

        public int getComponentSize(int v) {
            return getRoot(vertexNode[v]).size;
        }

        private final List<Edge> spanningEdges = new ArrayList<>();

        public void prepareSpanningEdges() {
            spanningEdges.clear();
            edgeTaken.clear();
        }

        public void getSpanningEdges(Node root) {
            if (root == null) {
                return;
            }
            if (!root.hasEdge) {
                return;
            }
            if (root.isHasEdge()) {
                Edge e = root.myEdge;
                if (!edgeTaken.contains(e)) { // It could be put 2 times, direct or inverse
                    edgeTaken.add(e);
                    spanningEdges.add(e);
                }
            }
            getSpanningEdges(root.l);
            getSpanningEdges(root.r);
        }

        private final List<Edge> allEdges = new ArrayList<>();

        public void prepareAllEdges() {
            allEdges.clear();
            edgeTaken.clear();
        }

        public Edge getAllEdges(Node root) {
            if (root == null) {
                return null;
            }
            if (!root.hasVertex) {
                return null;
            }
            if (root.isHasVertex()) {
                for (Edge e : adjacent[root.id][root.level]) {
                    if (isConnected(e.u, e.v)) {
                        if (!edgeTaken.contains(e)) {
                            edgeTaken.add(e);
                            allEdges.add(e);
                        }
                    } else {
                        return e;
                    }
                }
            }
            Edge tmp = getAllEdges(root.l);
            if (tmp != null) {
                return tmp;
            }
            return getAllEdges(root.r);
        }

        public boolean isConnected(int u, int v) {
            Node r1 = getRoot(vertexNode[u]);
            Node r2 = getRoot(vertexNode[v]);
            return r1 == r2;
        }
    }

    private final int N;
    private final Forest[] forest;
    private final HashSet<Edge>[][] adjacent;
    private final HashMap<Edge, Integer> edgeIndex; // id by edge
    private final HashSet<Edge> edgeTaken; // is the edge was taken into consideration previously

    private int connectedComponents;
    private final Edge[] mirrorEdges;

    public AksenovDynamicGraph(int n, DynamicGraph.Edge[] incomingEdges) {
        N = n;

        mirrorEdges = new Edge[incomingEdges.length];

        connectedComponents = n;
        int p = 1;
        int k = 1;
        while (p <= n) {
            p *= 2;
            k++;
        }

        //noinspection unchecked
        adjacent = new HashSet[n][k];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < k; j++) {
                adjacent[i][j] = new HashSet<>();
            }
        }

        forest = new Forest[k];
        for (int i = 0; i < k; i++) {
            forest[i] = new Forest(n, i);
        }

        edgeIndex = new HashMap<>();
        edgeTaken = new HashSet<>();
    }

    public void clear() {
        connectedComponents = N;
        Arrays.fill(mirrorEdges, null);

        for (int i = 0; i < forest.length; i++) {
            forest[i] = new Forest(N, i);
        }

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < forest.length; j++) {
                adjacent[i][j].clear();
            }
        }

        edgeIndex.clear();
        edgeTaken.clear();
    }

    public int numberOfCC() {
        return connectedComponents;
    }

    public boolean isConnected(int u, int v) {
        return forest[0].isConnected(u, v);
    }

    public boolean addEdge(DynamicGraph.Edge theEdge) {
        int u = theEdge.vertexA();
        int v = theEdge.vertexB();
        assert u < v;
        int id = theEdge.id();

        if (mirrorEdges[id] != null) {
            return false;
        }
        Edge e = new Edge(u, v);
        mirrorEdges[id] = e;
        edgeIndex.put(e, id);

        if (!forest[0].isConnected(u, v)) { // If this is a spanning tree
            forest[0].link(e); // link two forest trees together
            connectedComponents--;
        } else {
            adjacent[u][0].add(e); // simply add to adjacency list on level 0 and update hasVertex and hasEdge
            adjacent[v][0].add(e);

            forest[0].updateToTop(forest[0].vertexNode[u]);
            forest[0].updateToTop(forest[0].vertexNode[v]);
        }

        assert checkState();
        return true;
    }

    private void increaseLevel(Edge edge, boolean spanning) {
        int u = edge.u;
        int v = edge.v;
        int level = edge.level;
        edge.level++;
        if (spanning) {
            NodePair p = forest[level].nodeByEdge.get(new Edge(u, v));
            assert p.a != null;
            forest[level].updateToTop(p.a);
            assert p.b != null;
            forest[level].updateToTop(p.b);
            forest[level + 1].link(edge);
        } else {
            adjacent[u][level].remove(edge);
            forest[level].updateToTop(forest[level].vertexNode[u]);
            adjacent[v][level].remove(edge);
            forest[level].updateToTop(forest[level].vertexNode[v]);

            adjacent[u][level + 1].add(edge);
            forest[level + 1].updateToTop(forest[level + 1].vertexNode[u]);
            adjacent[v][level + 1].add(edge);
            forest[level + 1].updateToTop(forest[level + 1].vertexNode[v]);

            assert forest[level + 1].isConnected(u, v);
        }
    }

    public boolean removeEdge(DynamicGraph.Edge theEdge) {
        int u = theEdge.vertexA();
        int v = theEdge.vertexB();
        int id = theEdge.id();
        assert u < v;

        Integer tmpId = edgeIndex.get(new Edge(u, v));

        if (tmpId == null) {
            return false;
        }
        assert id == tmpId;

        Edge e = mirrorEdges[id];
        mirrorEdges[id] = null;

        int rank = e.level;

        if (!forest[0].nodeByEdge.containsKey(e)) { // The edge is not in the spanning tree
            adjacent[u][rank].remove(e); // simply remove from the adjacency list on level `level`
            adjacent[v][rank].remove(e);

            forest[rank].updateToTop(forest[rank].vertexNode[u]);
            forest[rank].updateToTop(forest[rank].vertexNode[v]);

            edgeIndex.remove(e);
            return true;
        }

        for (int level = rank; level >= 0; level--) {
            forest[level].cut(e);
        }

        assert !isConnected(u, v);

        boolean replaced = false;
        for (int level = rank; level >= 0; level--) {
            int w = (forest[level].getComponentSize(u) > forest[level].getComponentSize(v))
                    ? v : u; // Choose the smallest component

            forest[level].prepareSpanningEdges();
            forest[level].getSpanningEdges(getRoot(forest[level].vertexNode[w]));
            for (Edge x : forest[level].spanningEdges) {
                assert !forest[level + 1].isConnected(u, v);
                increaseLevel(x, true);
            }

            forest[level].prepareAllEdges();
            Edge good = forest[level].getAllEdges(getRoot(forest[level].vertexNode[w]));
            for (Edge x : forest[level].allEdges) {
                increaseLevel(x, false);
            }

            if (good != null) { // We found good edge
                adjacent[good.u][level].remove(good);
                adjacent[good.v][level].remove(good);
                forest[level].updateToTop(forest[level].vertexNode[good.u]);
                forest[level].updateToTop(forest[level].vertexNode[good.v]);

                for (int i = level; i >= 0; i--) {
                    forest[i].link(good);
                }

                replaced = true;
                break;
            }
        }

        if (!replaced) {
            connectedComponents++;
        }

        edgeIndex.remove(e);

        assert checkState();

        return true;
    }

    private boolean checkState() {
        if (CRAZY_ASSERTIONS) {
            for (int level = 0; level < forest.length; level++) {
                for (int v = 0; v < N; v++) {
                    for (Edge e : adjacent[v][level]) {
                        if (e.level != level) return false;
                        if (!forest[level].isConnected(e.u, e.v)) return false;
                    }
                }

                for (Edge e : forest[level].nodeByEdge.keySet()) {
                    if (!forest[level].isConnected(e.u, e.v)) return false;
                    if (level > 0 && !forest[level - 1].nodeByEdge.containsKey(e)) return false;
                }
            }
        }
        return true;
    }
}
