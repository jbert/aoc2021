package aoc

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/jbert/aoc2021/fun"
	"github.com/jbert/aoc2021/pts"
	"github.com/jbert/aoc2021/set"
)

type Day19 struct{}

func NewDay19() *Day19 {
	d := Day19{}
	return &d
}

func (d *Day19) Run(out io.Writer, lines []string) error {
	var scanners []*Scanner
	for len(lines) > 0 {
		var scanner *Scanner
		scanner, lines = readScanner(lines)
		scanners = append(scanners, scanner)
		fmt.Fprintf(out, "%s\n", scanner)
		//		fmt.Fprintf(out, "%d lines left\n", len(lines))
	}
	// Want 12 beacons overlap, that's 12*11/2=66 same distances
	for i, a := range scanners {
		for _, b := range scanners[i+1:] {
			intersects := a.Dists().Intersect(b.Dists())
			fmt.Printf("%d - %d: %d int %d\n", a.id, b.id, a.Dists().Size(), intersects.Size())
			if a.ScanOverlap(b) {
				fmt.Printf("%d:%d - Found overlap\n", a.id, b.id)
			}
		}
	}
	return nil
}

type Scanner struct {
	id      int
	beacons BSet

	known    bool
	location pts.P3
}

type BSet struct {
	set.Set[pts.P3]
}

func NewBSet() BSet {
	return BSet{set.New[pts.P3]()}
}

func (bs *BSet) Deltas() BSet {
	l := bs.ToList()

	ds := NewBSet()
	for i, a := range l {
		for _, b := range l[i+1:] {
			d := a.Sub(b)
			ds.Insert(d)
			//			fmt.Printf("%s - %s = %s\n", a, b, d)
		}
	}
	return ds
}

func (s *Scanner) Dists() set.Set[int] {
	return set.Map(s.beacons.Deltas().Set, func(p pts.P3) int { return p.ManhattanLength() })
}

func (s *Scanner) ScanOverlap(t *Scanner) bool {
	for _, sbs := range s.BeaconSets() {
		for _, tbs := range t.BeaconSets() {
			intersect := sbs.Deltas().Set.Intersect(tbs.Deltas().Set)
			if intersect.Size() >= 12*11/2 {
				return true
			}
		}
	}
	return false
}

func (s *Scanner) BeaconSets() []BSet {
	var bss []BSet
	for _, rot := range rotations {
		bs := set.Map(s.beacons.Set, rot)
		bss = append(bss, BSet{bs})
	}
	return bss
}

func (s *Scanner) String() string {
	b := &strings.Builder{}
	fmt.Fprintf(b, "Scanner %d has %d beacons\n", s.id, s.beacons.Size())
	for _, beacon := range s.beacons.ToList() {
		fmt.Fprintf(b, "%s\n", beacon)
	}
	return b.String()
}

func readScanner(lines []string) (*Scanner, []string) {
	/*
		fmt.Printf("LINES:\n")
		for _, l := range lines {
			fmt.Printf("L:%s\n", l)
		}
	*/
	rest := strings.TrimPrefix(lines[0], "--- scanner ")
	if rest == lines[0] {
		panic(fmt.Sprintf("Bad scanner start line: [%s]", lines[0]))
	}
	bits := strings.Split(rest, " ")

	s := &Scanner{}
	var err error
	s.id, err = strconv.Atoi(bits[0])
	if err != nil {
		panic(fmt.Sprintf("Bad scanner id:  bit [%s] line: [%s]", bits[0], lines[0]))
	}

	var beacons []pts.P3
	var i int
	var line string
LINES:
	for i, line = range lines[1:] {
		if strings.HasPrefix(line, "---") {
			i--
			break LINES
			return s, lines[i:]
		}
		beacon := pts.P3FromString(line)
		beacons = append(beacons, beacon)
	}
	i++
	s.beacons = BSet{set.SetFromList(beacons)}
	return s, lines[i+1:]
}

var rotations = makeRotations()

func makeRotations() []func(pts.P3) pts.P3 {
	// 6 directions, 4 orientations

	id := func(a pts.P3) pts.P3 { return a }
	rotX := func(a pts.P3) pts.P3 { return pts.P3{a.X, -a.Z, a.Y} }
	rotY := func(a pts.P3) pts.P3 { return pts.P3{-a.Z, a.Y, a.X} }
	rotZ := func(a pts.P3) pts.P3 { return pts.P3{-a.Y, a.X, a.Z} }

	// Put each of the 6 directions (x, -x, y, -y, z, -z) in the +x dir
	// then have the 4 rotations of that

	// Composing will be useful
	c := func(fs ...func(pts.P3) pts.P3) func(pts.P3) pts.P3 {
		return func(a pts.P3) pts.P3 {
			for _, f := range fun.Reverse(fs) {
				a = f(a)
			}
			return a
		}
	}

	xRots := []func(pts.P3) pts.P3{
		id,
		rotX,
		c(rotX, rotX),
		c(rotX, rotX, rotX),
	}

	sixDirs := []func(pts.P3) pts.P3{
		id,                  // +x
		c(rotX, rotX),       // -x
		rotZ,                // +y
		c(rotZ, rotZ, rotZ), // -y
		rotY,                // +z
		c(rotY, rotY, rotY), // -z
	}

	var perms []func(pts.P3) pts.P3
	for _, xrot := range xRots {
		for _, dir := range sixDirs {
			perms = append(perms, c(dir, xrot))
		}
	}

	return perms
}
