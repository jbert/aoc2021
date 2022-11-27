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
	scanners[0].known = true
	scanners[0].iRot = 0
	scanners[0].location = pts.P3{0, 0, 0}

	beacons := scanners[0].beacons.Set

	for i, a := range scanners {
		for _, b := range scanners[i+1:] {
			// intersects := a.Dists().Intersect(b.Dists())
			// Want 12 beacons overlap, that's 12*11/2=66 same distances
			//			if intersects.Size() > 12*11/2 {
			//				fmt.Printf("%d - %d: %d int %d\n", a.id, b.id, a.Dists().Size(), intersects.Size())
			//			}
			if ii, jj, ok := a.ScanOverlap(b); ok {
				fmt.Printf("%d:%d - Found overlap for i %d j %d\n", a.id, b.id, ii, jj)
				b.SetLocationOrientation(a, ii, jj)
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
	iRot     int
}

type BSet struct {
	set.Set[pts.P3]
}

func NewBSet() BSet {
	return BSet{set.New[pts.P3]()}
}

func (bs *BSet) Deltas() BSet {
	bsl := bs.ToList()

	ds := NewBSet()
	for _, a := range bsl {
		for _, b := range bsl {
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

func (s *Scanner) ScanOverlap(t *Scanner) (int, int, bool) {
	for i, sbs := range s.BeaconSets() {
		for j, tbs := range t.BeaconSets() {
			intersect := sbs.Deltas().Set.Intersect(tbs.Deltas().Set)
			//			fmt.Printf("SO %d - %d: (%d) int %d\n", s.id, t.id, sbs.Deltas().Set.Size(), intersect.Size())
			//			fmt.Printf("SO %d - %d: (%d) int %d\n", s.id, t.id, sbs.Deltas().Set.Size(), intersect.Size())
			if intersect.Size() >= 12*11/2 {
				return i, j, true
			}
		}
	}
	return 0, 0, false
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

type Rot func(pts.P3) pts.P3

var (
	id   = func(a pts.P3) pts.P3 { return a }
	rotX = func(a pts.P3) pts.P3 { return pts.P3{a.X, -a.Z, a.Y} }
	rotY = func(a pts.P3) pts.P3 { return pts.P3{a.Z, a.Y, -a.X} }
	rotZ = func(a pts.P3) pts.P3 { return pts.P3{-a.Y, a.X, a.Z} }
	c    = func(fs ...func(pts.P3) pts.P3) func(pts.P3) pts.P3 {
		return func(a pts.P3) pts.P3 {
			for _, f := range fun.Reverse(fs) {
				a = f(a)
			}
			return a
		}
	}
	xRots = []func(pts.P3) pts.P3{
		id,
		rotX,
		c(rotX, rotX),
		c(rotX, rotX, rotX),
	}

	sixDirs = []func(pts.P3) pts.P3{
		id,                  // +x
		c(rotZ, rotZ),       // -x
		rotZ,                // +y
		c(rotZ, rotZ, rotZ), // -y
		rotY,                // +z
		c(rotY, rotY, rotY), // -z
	}
)

var rotations = makeRotations()

func makeRotations() []func(pts.P3) pts.P3 {
	// 6 directions, 4 orientations

	// Put each of the 6 directions (x, -x, y, -y, z, -z) in the +x dir
	// then have the 4 rotations of that

	var perms []func(pts.P3) pts.P3
	for _, xrot := range xRots {
		for _, dir := range sixDirs {
			perm := c(xrot, dir)
			//			p := pts.P3{1, 2, 3}
			//			fmt.Printf("%d,%d p -> %s\n", i, j, perm(p))
			perms = append(perms, perm)
		}
	}

	return perms
}
