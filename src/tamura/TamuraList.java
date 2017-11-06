package tamura;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;

import graph.Edge;

public class TamuraList extends LinkedList<Edge> {

	private static final long serialVersionUID = 1L;
	
	@Override
	public boolean add(Edge e_k) {
		int i = 0;
		for (Edge e : this) {
			if (Tamura.dplus(e_k) < Tamura.dplus(e)) 
				break;
			i++;
		}
		super.add(i, e_k);
		return true;
	}
	
	@Override
	public boolean addAll(Collection<? extends Edge> c) {
		super.addAll(c);
		Collections.sort(this, Comparator.comparingInt(Tamura::dplus));
		return true;
	}

}
