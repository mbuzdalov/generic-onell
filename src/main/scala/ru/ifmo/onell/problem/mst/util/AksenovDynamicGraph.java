package ru.ifmo.onell.problem.mst.util;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Implementation of dynamic connectivity based on a hierarchy of partial forests.
 *
 * Source: <a href="https://github.com/Aksenov239/concurrent-graph/blob/master/src/sequential/SequentialDynamicGraph.java">Aksenov239/concurrent-graph</a>.
 *
 * @author Vitaly Aksenov
 */
public final class AksenovDynamicGraph {
    private static final boolean CRAZY_ASSERTIONS = false;

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

    private static class Node {
        Node l, r, p;
        final int y;
        int size;
        final Edge myEdge;
        final Set<Edge> myAdjacentEdges;
        final int levelOrId;
        boolean hasVertexInSubtree;
        boolean hasEdgeInSubtree;

        public Node(int id, Set<Edge> myAdjacentEdges) {
            y = ThreadLocalRandom.current().nextInt();
            this.myAdjacentEdges = myAdjacentEdges;
            this.levelOrId = id;
            this.myEdge = null;
            update();
        }

        public Node(Edge edge, int level) {
            y = ThreadLocalRandom.current().nextInt();
            this.myAdjacentEdges = Collections.emptySet();
            this.levelOrId = level;
            this.myEdge = edge;
            update();
        }

        public void update() {
            hasVertexInSubtree = !getEdgesAdjacentToVertex().isEmpty();
            hasEdgeInSubtree = getAssociatedEdge() != null;
            size = 1;
            if (l != null) {
                l.p = this;
                hasVertexInSubtree |= l.hasVertexInSubtree;
                hasEdgeInSubtree |= l.hasEdgeInSubtree;
                size += l.size;
            }
            if (r != null) {
                r.p = this;
                hasVertexInSubtree |= r.hasVertexInSubtree;
                hasEdgeInSubtree |= r.hasEdgeInSubtree;
                size += r.size;
            }
        }

        public Set<Edge> getEdgesAdjacentToVertex() {
            return myAdjacentEdges;
        }

        public Edge getAssociatedEdge() {
            if (myEdge != null) {
                return myEdge.level == levelOrId ? myEdge : null;
            }
            return null;
        }

        public String toString() {
            String me;
            if (myEdge != null) {
                me = myEdge.u + "->" + myEdge.v;
            } else {
                me = String.valueOf(levelOrId);
            }

            return "[" + (l == null ? "" : l + ",") + me + (r == null ? "" : "," + r) + "]";
        }
    }

    private static int getSizeNode(Node node) {
        return node == null ? 0 : node.size;
    }

    private static void updateToTop(Node v) {
        while (v != null) {
            v.update();
            v = v.p;
        }
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
        Set<Edge>[] adjacent;
        Node[] splitTemp = new Node[2];

        public Forest(int n, int level) {
            this.level = level;
            //noinspection unchecked
            this.adjacent = new Set[n];
            nodeByEdge = new HashMap<>();
            vertexNode = new Node[n];
            for (int i = 0; i < n; i++) {
                adjacent[i] = new HashSet<>();
                vertexNode[i] = new Node(i, adjacent[i]);
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

            Node c1 = new Node(e, level);
            Node c2 = new Node(e, level);
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
            if (!root.hasEdgeInSubtree) {
                return;
            }
            Edge e = root.getAssociatedEdge();
            if (e != null) {
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
            if (!root.hasVertexInSubtree) {
                return null;
            }
            for (Edge e : root.getEdgesAdjacentToVertex()) {
                if (isConnected(e.u, e.v)) {
                    if (!edgeTaken.contains(e)) {
                        edgeTaken.add(e);
                        allEdges.add(e);
                    }
                } else {
                    return e;
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

        forest = new Forest[k];
        for (int i = 0; i < k; i++) {
            forest[i] = new Forest(n, i);
        }

        edgeTaken = new HashSet<>();
    }

    public void clear() {
        connectedComponents = N;
        Arrays.fill(mirrorEdges, null);

        for (int i = 0; i < forest.length; i++) {
            forest[i] = new Forest(N, i);
        }

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
        Forest f0 = forest[0];

        if (!f0.isConnected(u, v)) { // If this is a spanning tree
            f0.link(e); // link two forest trees together
            connectedComponents--;
        } else {
            f0.adjacent[u].add(e); // simply add to adjacency list on level 0 and update hasVertex and hasEdge
            f0.adjacent[v].add(e);

            updateToTop(f0.vertexNode[u]);
            updateToTop(f0.vertexNode[v]);
        }

        assert checkState();
        return true;
    }

    private void increaseLevel(Edge edge, boolean spanning) {
        int u = edge.u;
        int v = edge.v;
        int level = edge.level;
        edge.level++;
        Forest fCurr = forest[level];
        Forest fNext = forest[level + 1];
        if (spanning) {
            NodePair p = fCurr.nodeByEdge.get(edge);
            updateToTop(p.a);
            updateToTop(p.b);
            fNext.link(edge);
        } else {
            fCurr.adjacent[u].remove(edge);
            updateToTop(fCurr.vertexNode[u]);
            fCurr.adjacent[v].remove(edge);
            updateToTop(fCurr.vertexNode[v]);

            fNext.adjacent[u].add(edge);
            updateToTop(fNext.vertexNode[u]);
            fNext.adjacent[v].add(edge);
            updateToTop(fNext.vertexNode[v]);

            assert fNext.isConnected(u, v);
        }
    }

    public boolean removeEdge(DynamicGraph.Edge theEdge) {
        int u = theEdge.vertexA();
        int v = theEdge.vertexB();
        int id = theEdge.id();
        assert u < v;

        Edge e = mirrorEdges[id];
        if (e == null) {
            return false;
        }
        mirrorEdges[id] = null;

        int rank = e.level;

        if (!forest[0].nodeByEdge.containsKey(e)) { // The edge is not in the spanning tree
            Forest fr = forest[rank];
            fr.adjacent[u].remove(e); // simply remove from the adjacency list on level `level`
            fr.adjacent[v].remove(e);

            updateToTop(fr.vertexNode[u]);
            updateToTop(fr.vertexNode[v]);
            return true;
        }

        for (int level = rank; level >= 0; level--) {
            forest[level].cut(e);
        }

        assert !isConnected(u, v);

        boolean replaced = false;
        for (int level = rank; level >= 0; level--) {
            Forest fCurr = this.forest[level];
            int w = (fCurr.getComponentSize(u) > fCurr.getComponentSize(v)) ? v : u; // Choose the smallest component

            fCurr.prepareSpanningEdges();
            fCurr.getSpanningEdges(getRoot(fCurr.vertexNode[w]));
            for (Edge x : fCurr.spanningEdges) {
                assert !forest[level + 1].isConnected(u, v);
                increaseLevel(x, true);
            }

            fCurr.prepareAllEdges();
            Edge good = fCurr.getAllEdges(getRoot(fCurr.vertexNode[w]));
            for (Edge x : fCurr.allEdges) {
                increaseLevel(x, false);
            }

            if (good != null) { // We found good edge
                fCurr.adjacent[good.u].remove(good);
                fCurr.adjacent[good.v].remove(good);
                updateToTop(fCurr.vertexNode[good.u]);
                updateToTop(fCurr.vertexNode[good.v]);

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

        assert checkState();

        return true;
    }

    private boolean checkState() {
        if (CRAZY_ASSERTIONS) {
            for (int level = 0; level < forest.length; level++) {
                for (int v = 0; v < N; v++) {
                    for (Edge e : forest[level].adjacent[v]) {
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
