package aoc

import (
	"fmt"
	"io"
	"strings"
)

type Day12 struct{}

type Vertex string

type Edge struct {
	a, b Vertex
}

func (e Edge) String() string {
	return fmt.Sprintf("%s - %s", e.a, e.b)
}

func (e Edge) Reverse() Edge {
	return Edge{e.b, e.a}
}

//type VertexSet Set[Vertex]

type Graph map[Vertex]Set[Vertex]

func (g Graph) String() string {
	b := &strings.Builder{}
	for v, s := range g {
		fmt.Fprintf(b, "%s:\t%s\n", v, s)
	}
	return b.String()
}

func NewGraphFromEdges(edges []Edge) *Graph {
	g := Graph{}
	for _, edge := range edges {
		g.addEdge(edge)
		g.addEdge(edge.Reverse())
	}
	return &g
}

func (g Graph) addEdge(e Edge) {
	s, ok := g[e.a]
	if !ok {
		s = NewSet[Vertex]()
	}
	s.Insert(e.b)
	g[e.a] = s
}

func lineToEdge(line string) Edge {
	bits := strings.Split(line, "-")
	if len(bits) != 2 {
		panic(fmt.Sprintf("Bad edge-line [%s]", line))
	}
	return Edge{Vertex(bits[0]), Vertex(bits[1])}
}

func NewDay12() *Day12 {
	d := Day12{}
	return &d
}

func (d *Day12) Run(out io.Writer, lines []string) error {
	fmt.Fprintf(out, "Running\n")
	edges := Map(lineToEdge, lines)
	fmt.Fprintf(out, "Edges: %v\n", edges)
	g := NewGraphFromEdges(edges)
	fmt.Fprintf(out, "G:\n%v\n", g)
	return nil
}
