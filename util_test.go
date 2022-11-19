package aoc

import (
	"sort"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSet(t *testing.T) {
	a := assert.New(t)
	s := NewSet[int]()

	a.True(s.IsEmpty(), "Nil set is empty")

	a.True(SetFromList([]int{1, 2, 3}).Equals(SetFromList([]int{3, 2, 1})), "order doesn't matter")

	xs := []int{1, 3, 5, 8}
	x := SetFromList(xs)
	a.Equal(x.Size(), len(xs), "Set same length as list")
	l := x.ToList()
	sort.Ints(l)
	a.Equal(l, xs, "ToList gives the right list")

	x.InsertList(xs)
	a.Equal(x.Size(), len(xs), "Inserting same list twice doesn't change set")

	dups := []int{3, 5, 3, 7, 3, 11}
	undups := []int{3, 5, 7, 11}
	d := SetFromList(dups)
	a.Equal(d.Size(), len(undups), "Sets don't contain duplicates")

	ud := SetFromList(undups)
	a.True(d.Equals(ud), "Set of dups and undups equal")

	a.True(x.Equals(x), "Set equals itself")

	u := s.Union(s)
	a.True(s.Equals(u), "Self union doesn't change the set")

	i := s.Intersect(s)
	a.True(s.Equals(i), "Self intersect doesn't change the set")

	ys := []int{3, 5, 7, -1, 100}
	y := SetFromList(ys)
	a.Equal(x.Intersect(y), SetFromList([]int{3, 5}), "intersect")
	a.Equal(x.Union(y), SetFromList([]int{1, 3, 5, 7, 8, -1, 100}), "union")
}
