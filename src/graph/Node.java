package graph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class Node {
	public int idx;
	
	public List<Edge> edges = new ArrayList<>();
	
	public Collection<Edge> getEdges() {
		return edges;
	}
	
	@Override
	public String toString() {
		return String.format("%d", idx);
	}
}
