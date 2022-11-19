package aoc

import (
	"fmt"
	"io"
	"strings"
)

type Day12 struct{}

type Vertex string

func (v Vertex) IsSmall() bool {
	if v == "" {
		panic("Empty string vertex")
	}
	return string(v) == strings.ToLower(string(v))
}

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

func (g Graph) CountPaths(from, to Vertex) int {
	if !g.IsVertex(from) || !g.IsVertex(to) {
		panic(fmt.Sprintf("Internal error: [%s] or [%s] not a vertex", from, to))
	}
	visited := make(map[Vertex]int)
	return g.countPaths(from, to, visited)
}

func (g Graph) countPaths(from, to Vertex, visited map[Vertex]int) int {
	if from == to {
		return 1
	}
	visited[from]++
	neighbours := g.Neighbours(from)

	canVisit := func(v Vertex) bool {
		if v.IsSmall() {
			return visited[v] == 0
		}
		return true
	}

	neighbours = Filter(canVisit, neighbours)

	count := 0
	for _, neighbour := range neighbours {
		visitedCopy := make(map[Vertex]int)
		for k, v := range visited {
			visitedCopy[k] = v
		}
		count += g.countPaths(neighbour, to, visitedCopy)
	}
	//	fmt.Printf("[%s -> %s]: neighbours [%s] returning %d\n", from, to, neighbours, count)
	return count
}

func NewGraphFromEdges(edges []Edge) *Graph {
	g := Graph{}
	for _, edge := range edges {
		g.addEdge(edge)
		g.addEdge(edge.Reverse())
	}
	return &g
}

func (g Graph) Neighbours(v Vertex) []Vertex {
	return g[v].ToList()
}

func (g Graph) IsVertex(v Vertex) bool {
	_, ok := g[v]
	return ok
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
	numPaths := g.CountPaths("start", "end")
	fmt.Fprintf(out, "Num paths: %d\n", numPaths)
	return nil
}
