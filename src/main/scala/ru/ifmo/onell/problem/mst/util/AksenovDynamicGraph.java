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

    private static class Edge {
        final int u, v;
        int level, dfsCounter;

        public Edge(int u, int v) {
            assert u != v;
            this.u = u;
            this.v = v;
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
        int size;
        final Edge myEdge;
        final Set<Edge> myAdjacentEdges;
        final int levelOrId;
        boolean hasVertexInSubtree;
        boolean hasEdgeInSubtree;

        public Node(int id, Set<Edge> myAdjacentEdges) {
            this.myAdjacentEdges = myAdjacentEdges;
            this.levelOrId = id;
            this.myEdge = null;
            update();
        }

        public Node(Edge edge, int level) {
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
                hasVertexInSubtree |= l.hasVertexInSubtree;
                hasEdgeInSubtree |= l.hasEdgeInSubtree;
                size += l.size;
            }
            if (r != null) {
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

    private static void zigRight(Node p, Node g) {
        Node pr = p.r;
        g.l = pr;
        if (pr != null) {
            pr.p = g;
        }
        p.r = g;
        g.p = p;
    }

    private static void zigLeft(Node p, Node g) {
        Node pl = p.l;
        g.r = pl;
        if (pl != null) {
            pl.p = g;
        }
        p.l = g;
        g.p = p;
    }

    private static void splay(Node x) {
        Node p, g;
        while ((p = x.p) != null) {
            if ((g = p.p) == null) {
                // Zig
                if (p.l == x) {
                    zigRight(x, p);
                } else {
                    zigLeft(x, p);
                }
                x.p = null;
            } else {
                Node gg = g.p;
                if (gg != null) {
                    g.p = null;
                    if (gg.l == g) {
                        gg.l = x;
                    } else {
                        gg.r = x;
                    }
                }
                if (p.l == x) {
                    if (g.l == p) {
                        // Zig-zig
                        zigRight(p, g);
                    } else {
                        // Zig-zag
                        zigLeft(x, g);
                    }
                    zigRight(x, p);
                } else {
                    if (g.r == p) {
                        // Zig-zig
                        zigLeft(p, g);
                    } else {
                        // Zig-zag
                        zigRight(x, g);
                    }
                    zigLeft(x, p);
                }
                x.p = gg;
                g.update();
            }
            p.update();
            x.update();
        }
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

        while (l.r != null) {
            l = l.r;
        }
        splay(l);
        l.r = r;
        r.p = l;
        l.update();
        return l;
    }

    private static Node getRoot(Node v) {
        while (v.p != null) {
            v = v.p;
        }
        return v;
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
            splay(v);
            Node l = v.l;
            if (l != null) {
                v.l = null;
                l.p = null;
                v.update();
                merge(v, l);
                splay(v);
            }
        }

        public void link(Edge e) {
            int u = e.u;
            int v = e.v;

            Node n1 = vertexNode[u];
            Node n2 = vertexNode[v];
            makeFirst(vertexNode[u]);
            makeFirst(vertexNode[v]);

            Node c1 = new Node(e, level);
            Node c2 = new Node(e, level);
            nodeByEdge.put(e, new NodePair(c1, c2));

            merge(merge(c2, n1), merge(c1, n2));
        }

        public void cut(Edge e) {
            NodePair c = nodeByEdge.remove(e);
            Node ca = c.a;
            Node cb = c.b;
            makeFirst(cb);

            cb.r.p = null;
            cb.r = null;
            splay(ca);
            ca.l.p = null;
            ca.l = null;
            ca.r.p = null;
            ca.r = null;
        }

        public int getComponentSize(int v) {
            return getRoot(vertexNode[v]).size;
        }

        public void increaseLevelOnSpanningEdges(Node root) {
            if (root == null || !root.hasEdgeInSubtree) {
                return;
            }
            Edge e = root.getAssociatedEdge();
            if (e != null) {
                if (e.dfsCounter == 0) {
                    increaseLevel(e, true);
                }
                ++e.dfsCounter;
            }
            increaseLevelOnSpanningEdges(root.l);
            increaseLevelOnSpanningEdges(root.r);
            if (e != null) {
                --e.dfsCounter;
            }
        }

        private final List<Edge> allEdges = new ArrayList<>();

        public void prepareAllEdges() {
            allEdges.clear();
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
                    if (e.dfsCounter == 0) {
                        ++e.dfsCounter;
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

        public void addNonSpanningEdge(Edge e) {
            int u = e.u;
            int v = e.v;
            adjacent[u].add(e);
            adjacent[v].add(e);
            updateToTop(vertexNode[u]);
            updateToTop(vertexNode[v]);
        }

        public void removeNonSpanningEdge(Edge e) {
            int u = e.u;
            int v = e.v;
            adjacent[u].remove(e);
            adjacent[v].remove(e);
            updateToTop(vertexNode[u]);
            updateToTop(vertexNode[v]);
        }
    }

    private final int N;
    private final Forest[] forest;
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
    }

    public void clear() {
        connectedComponents = N;
        Arrays.fill(mirrorEdges, null);

        for (int i = 0; i < forest.length; i++) {
            forest[i] = new Forest(N, i);
        }
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

        if (isConnected(u, v)) {
            f0.addNonSpanningEdge(e);
        } else {
            f0.link(e); // link two forest trees together
            connectedComponents--;
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
            fCurr.removeNonSpanningEdge(edge);
            fNext.addNonSpanningEdge(edge);

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
            fr.removeNonSpanningEdge(e);
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

            fCurr.increaseLevelOnSpanningEdges(getRoot((fCurr.vertexNode[w])));

            fCurr.prepareAllEdges();
            Edge good = fCurr.getAllEdges(getRoot(fCurr.vertexNode[w]));
            for (Edge x : fCurr.allEdges) {
                increaseLevel(x, false);
                x.dfsCounter = 0;
            }

            if (good != null) { // We found good edge
                fCurr.removeNonSpanningEdge(good);

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
