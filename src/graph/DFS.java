package graph;

import java.util.ArrayDeque;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

public class DFS {
	
	private Set<Node> 			explored	= new HashSet<>();
	private Set<Edge>			used		= new HashSet<>();
	private Set<Edge> 			frontierSet = new HashSet<>();
	private ArrayDeque<Edge>	frontier 	= new ArrayDeque<>();

	public void addToExplored(Node n) {
		this.explored.add(n);
	}

	public boolean isExplored(Node n) {
		return this.explored.contains(n);
	}
	
	public Node getUnexplored(Edge e) {
		return !this.isExplored(e.n1) ? e.n1 : e.n2;
	}

	public void addToUsed(Edge n) {
		this.used.add(n);
	}

	public boolean isUsed(Edge n) {
		return this.used.contains(n);
	}

	public boolean frontierIsEmpty() {
		return frontier.isEmpty();
	}

	public boolean inFrontier(Edge n) {
		return frontierSet.contains(n);
	}

	public Edge getAndRemoveLeaf() {
		Edge n = frontier.pop();
		frontierSet.remove(n);
		return n;
	}

	public void addToFrontier(Edge n) {
		frontier.push(n);
		frontierSet.add(n);
		used.add(n);
	}

	public static List<Edge> search(Node initial)
	{		
		List<Edge> traversal = new LinkedList<>();
		
		DFS strategy = new DFS();
		
		strategy.addToExplored(initial);
		
		initial.getEdges().forEach(e -> strategy.addToFrontier(e));
		
		while (!strategy.frontierIsEmpty())
		{
			Edge leaf = strategy.getAndRemoveLeaf();
			
			if (strategy.isExplored(leaf.n1) && strategy.isExplored(leaf.n2))
				continue;
			
			traversal.add(leaf);
			
			Node unexplored = strategy.getUnexplored(leaf);
			
			strategy.addToExplored(unexplored);
			
			for (Edge e : unexplored.getEdges())
			{
				if (!strategy.isUsed(e)) strategy.addToFrontier(e);
			}
		}
		return traversal;
	}
}
