package graph;

public class Edge {
	public int w, m, idx;
	public Node n1, n2;
	
	public Node getNeighbor(Node n) {
		return n == n1 ? n2 : n1;
	}
	
	public int getWeight() {
		return w;
	}
	
	public int getMirrorWeight() {
		return m;
	}
	
	@Override
	public String toString() {
		return String.format("(%s,%s,%d)", n1, n2, w);
	}
}
