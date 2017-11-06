package graph;

import java.io.*;
import java.util.*;

public class Graph
{
	public int 	edge_count, 
				node_count;	
	
	public Edge[] edges;
	public Node[] nodes;
	
	public Graph(String file) {

     try {
     	BufferedReader in = new BufferedReader(new FileReader(file));
			
			node_count = Integer.parseInt(in.readLine());
			edge_count = Integer.parseInt(in.readLine());
			
			edges = new Edge[edge_count];			
			for (int i = 0; i < edge_count; i++) edges[i] = new Edge();
			
			nodes = new Node[node_count];
			for (int i = 0; i < node_count; i++) nodes[i] = new Node();
			
			String line;
			
			int edge_index = 0;
			
			while ((line = in.readLine()) != null)
			{
				if (line.equals("")) continue;
				String[] input = line.split(" ");
				int n1 = Integer.parseInt(input[0]) - 1;
				int n2 = Integer.parseInt(input[1]) - 1;
				int w  = Integer.parseInt(input[2]);
				
				edges[edge_index] 					.w = w;
				edges[edge_count - edge_index - 1]	.m = w;
				
				edges[edge_index].n1 	= nodes[n1];
				edges[edge_index].n2 	= nodes[n2];
				
				nodes[n1].edges.add(edges[edge_index]);
				nodes[n2].edges.add(edges[edge_index]);
				
				nodes[n1].idx = n1;
				nodes[n2].idx = n2;
				
				edge_index++;
			}
			
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
     
	}
}

class MST
{
	public static void main(String[] args) {
		System.out.println(MST.run(new Graph("TestFiles\\TestFile1.uwg")));
	}
	
	public static int run(Graph g)
	{		
		Set<Node> unvisited = new HashSet<>();
		for (int i = 0; i < g.node_count; i++) unvisited.add(g.nodes[i]);
		
		Node v = unvisited.stream().findFirst().get();
		unvisited.remove(v);
		
		Queue<Edge> edge_queue = new PriorityQueue<>(Comparator.comparingInt(e -> Math.max(e.w + e.m, Math.max(e.w, e.m))));
		List<Edge> edge_path = new ArrayList<>();
		
		while (!unvisited.isEmpty())
		{
			for (Edge e : v.edges)
				if (unvisited.contains(v == e.n1 ? e.n2 : e.n1))
					edge_queue.add(e);
			
			Edge e = edge_queue.remove();
			edge_path.add(e);
			
			v = unvisited.contains(e.n2) ? e.n2 : e.n1;
			unvisited.remove(v);
		}
		
		int totalW = 0, totalM = 0;
		
		System.out.println(edge_path.size());
		
		for (Edge e : edge_path)
		{
			totalW += e.w;
			totalM += e.m;
		}
		
		return Math.max(totalW, totalM);
	}
}