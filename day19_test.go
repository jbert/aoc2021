package aoc

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestRots(t *testing.T) {
	a := assert.New(t)
	l := []string{
		"--- scanner 7 ---", "1,0,0", "0,1,0", "0,0,1",
	}
	s, l := readScanner(l)
	fmt.Printf("s is %s\n", s)
	a.Equal([]string{}, l, "consumed all lines")
	a.Equal(s.id, 7, "correct scanner id")
	a.Equal(s.beacons.Size(), 3, "three beacons")
	dists := s.Dists()
	fmt.Printf("dists: %v\n", dists)
}
