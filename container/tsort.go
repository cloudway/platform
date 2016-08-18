package container

import "errors"

type node struct {
	data       interface{}
	deps, sups []*node
}

func (n *node) dependsOn(m *node) {
	n.deps = append(n.deps, m)
	m.sups = append(m.sups, n)
}

var ErrCircularReference = errors.New("Circular reference detected")

// Kahn's algorithm. See https://en.wikipedia.org/wiki/Topological_sorting
func TSort(G []*node) ([]*node, error) {
	var S, L []*node
	var n, m *node

	for _, n = range G {
		if len(n.deps) == 0 {
			S = append(S, n)
		}
	}

	for len(S) != 0 {
		n, S = S[0], S[1:]
		for len(n.sups) != 0 {
			m = n.sups[0]
			n.sups = n.sups[1:]
			m.deps = remove(m.deps, n)
			if len(m.deps) == 0 {
				S = append(S, m)
			}
		}
		L = append(L, n)
	}

	for _, n = range G {
		if len(n.deps) != 0 {
			return G, ErrCircularReference
		}
	}

	return L, nil
}

func remove(vs []*node, n *node) []*node {
	for i, m := range vs {
		if n == m {
			return append(vs[0:i], vs[i+1:]...)
		}
	}
	return vs
}
