package tamura;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import graph.DFS;
import graph.Edge;
import graph.Graph;
import graph.Node;

public class Tamura {
	
	public static void main(String[] args) {
//		args = new String[] { "data\\TestFile4.uwg", "data\\TestFile5.uwg" };
		for (String arg : args) {
			long start 	= System.currentTimeMillis();
			int B 		= new Tamura().run(new Graph(arg));
			long end 	= System.currentTimeMillis();
			System.out.println(String.format("%s\tB:%4d\tTime:%5d", arg, B, end - start));
		}
	}
	
	public int B 						= Integer.MAX_VALUE;
	public Set<Edge> T0 				= new HashSet<>();
	public TamuraList leave 			= new TamuraList();
	public Map<Edge, TamuraList> candi 	= new HashMap<>();
	
	public int run(Graph g)
	{
		Node initial = g.nodes[0];

		List<Node> nodes = new ArrayList<>();
		List<Edge> edges = DFS.search(initial);
		
		initial.idx = 0;
		nodes.add(initial);		
		for (int i = 1; i < g.nodes.length; i++) {
			// Spanning tree consists of the first V - 1 edges
			Edge e = edges.get(i - 1);
			e.idx = i - 1;
			T0.add(e);
			// Numerate vertices in the order they are visited
			Node n = !nodes.contains(e.n1) ? e.n1 : e.n2;
			n.idx = i;
			nodes.add(n);
		}
		
		// Add edges not in spanning tree
		edges.addAll(Arrays.stream(g.edges)
				.filter(e -> !edges.contains(e))
				.sorted(Comparator.comparingInt(Tamura::dplus))
				.collect(Collectors.toList()));
		
		// Initialize candi
		edges.forEach(e -> candi.put(e, new TamuraList()));
		
		// Add candidates
		T0.forEach(e -> candi.get(e).addAll(Can(e, edges)));
		
		// Initialize leave
		leave.addAll(candi.entrySet().stream()
				.filter(entry -> !entry.getValue().isEmpty())
				.map(Entry::getKey)
				.collect(Collectors.toList()));
		
		output();
		find_child();
		return B;
	}
	
	public void find_child()
	{
		if (leave.isEmpty()) return;
		LinkedList<Edge> Q = new LinkedList<>();
		Edge e_k = leave.removeLast();
		while (!candi.get(e_k).isEmpty()) {
			Edge g = candi.get(e_k).removeLast();
			Q.addFirst(g);
			T0.remove(e_k);
			T0.add(g);
			output();
			sub_child(e_k, g);
			T0.add(e_k);
			T0.remove(g);
//			output();
		}
		candi.get(e_k).addAll(Q);
		sub_child(e_k, e_k);
		leave.addLast(e_k);
	}
	
	public void sub_child(Edge e_k, Edge g)
	{
		if (candi.get(e_k).isEmpty() || dplus(g) <= dplus(candi.get(e_k).getFirst())) {
			find_child();
			return;
		}
		Edge f = T0.stream().filter(e -> dminus(e) == dplus(g)).findAny().get();
		if (!candi.get(f).isEmpty()) {
			Set<Edge> S = candi.get(e_k).stream().filter(e -> dplus(e) < dplus(g)).collect(Collectors.toSet());
			candi.get(f).addAll(S);
			find_child();
			candi.get(f).removeAll(S);
		}
		else {
			candi.get(f).addAll(candi.get(e_k).stream().filter(e -> dplus(e) < dplus(g)).collect(Collectors.toSet()));
			leave.add(f);
			find_child();
			leave.remove(f);
			candi.get(f).clear();
		}
	}
	
	private Set<Edge> Can(Edge e_j, List<Edge> edges) {
		return edges.stream()
				.filter(e -> !T0.contains(e) 
						  && dminus(e) == dminus(e_j)
						  && dplus(e) <= dplus(e_j))
				.collect(Collectors.toSet());
	}
	
	private void output() {
		int b = Math.max(T0.stream().mapToInt(Edge::getWeight).sum(), 
						 T0.stream().mapToInt(Edge::getMirrorWeight).sum());
		if (b < B)
		{
			B = b;
			System.out.println(B);
		}
	}
	
	public static int dplus(Edge e) {
		return Math.min(e.n1.idx, e.n2.idx);
	}

	public static int dminus(Edge e) {
		return Math.max(e.n1.idx, e.n2.idx);
	}
}
