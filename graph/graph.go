package graph

import (
	"fmt"
	"strings"

	"github.com/jbert/aoc2021/set"
)

type Edge[V comparable] struct {
	From   V
	To     V
	Weight float64
}

func (e Edge[V]) String() string {
	return fmt.Sprintf("%v - %v", e.From, e.To)
}

func (e Edge[V]) Reverse() Edge[V] {
	return Edge[V]{From: e.To, To: e.From}
}

type Graph[V comparable] map[V]set.Set[V]

func (g Graph[V]) String() string {
	b := &strings.Builder{}
	for v, s := range g {
		fmt.Fprintf(b, "%v:\t%v\n", v, s)
	}
	return b.String()
}

func NewFromEdges[V comparable](edges []Edge[V], undirected bool) *Graph[V] {
	g := Graph[V]{}
	for _, edge := range edges {
		g.addEdge(edge)
		if undirected {
			g.addEdge(edge.Reverse())
		}
	}
	return &g
}

func (g Graph[V]) addEdge(e Edge[V]) {
	s, ok := g[e.From]
	if !ok {
		s = set.New[V]()
	}
	s.Insert(e.To)
	g[e.From] = s
}

func (g Graph[V]) Neighbours(v V) []V {
	return g[v].ToList()
}

func (g Graph[V]) IsVertex(v V) bool {
	_, ok := g[v]
	return ok
}
