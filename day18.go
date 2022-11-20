package aoc

import (
	"fmt"
	"io"
	"strconv"
	"strings"
	"unicode"

	"github.com/jbert/aoc2021/fun"
)

type Day18 struct{}

func NewDay18() *Day18 {
	d := Day18{}
	return &d
}

func (d *Day18) Run(out io.Writer, lines []string) error {
	snums := fun.Map(lineToSnum, lines)
	for _, snum := range snums {
		fmt.Printf("%s\n", snum)
	}
	total := snums[0]
	for _, snum := range snums[1:] {
		total = total.Add(snum)
	}
	fmt.Printf("Magnitude: %d\n", total.Magnitude())
	return nil
}

// No sum types in go
type Snum struct {
	isInt bool
	n     int
	l     *Snum
	r     *Snum
	up    *Snum
}

func (snum *Snum) Add(other *Snum) *Snum {
	total := &Snum{l: snum, r: other}
	total.Reduce()
	return total
}

func (snum *Snum) Reduce() {
AGAIN:
	for {
		if snum.checkDoExplode() {
			continue AGAIN
		}
		if snum.checkDoSplit() {
			continue AGAIN
		}
		break AGAIN
	}
	return
}

func (snum *Snum) checkDoExplode() bool {
	_, _, exploded := snum.explodeHelper(1)
	return exploded
}

func (snum *Snum) explodeHelper(depth int) (int, int, bool) {
	if snum.isInt {
		return 0, 0, false
	}

	if depth == 5 {
		if !snum.l.isInt || !snum.r.isInt {
			panic("Depth 6 snum found")
		}
		addLeft := snum.l.n
		addRight := snum.r.n

		snum.isInt = true
		snum.n = 0
		snum.l = nil
		snum.r = nil

		return addLeft, addRight, true
	}

	addLeft, addRight, exploded := snum.l.explodeHelper(depth + 1)
	if exploded {
		snum.r.addToLeftmost(addRight)
		return addLeft, 0, exploded
	}

	addLeft, addRight, exploded = snum.r.explodeHelper(depth + 1)
	if exploded {
		snum.l.addToRightmost(addLeft)
		return 0, addRight, exploded
	}

	return 0, 0, false
}

func (snum *Snum) addToLeftmost(n int) {
	if snum.isInt {
		snum.n += n
		return
	}
	snum.l.addToLeftmost(n)
}

func (snum *Snum) addToRightmost(n int) {
	if snum.isInt {
		snum.n += n
		return
	}
	snum.r.addToRightmost(n)
}

func (snum *Snum) checkDoSplit() bool {
	if snum.isInt {
		if snum.n >= 10 {
			snum.isInt = false
			snum.l = &Snum{isInt: true, n: snum.n / 2}
			snum.r = &Snum{isInt: true, n: (snum.n + 1) / 2}
			snum.n = 0
			return true
		}
		return false
	}
	if snum.l.checkDoSplit() {
		return true
	}
	if snum.r.checkDoSplit() {
		return true
	}
	return false
}

func (snum *Snum) Magnitude() int {
	if snum.isInt {
		return snum.n
	} else {
		return (3 * snum.l.Magnitude()) + (2 * snum.r.Magnitude())
	}
}

func (snum *Snum) String() string {
	if snum.isInt {
		return fmt.Sprintf("%d", snum.n)
	} else {
		return fmt.Sprintf("[%s,%s]", snum.l, snum.r)
	}
}

func lineToSnum(l string) *Snum {
	snum, rem := parseSnumLine(l)
	if rem != "" {
		panic(fmt.Sprintf("Failed parse of [%s] rem %s", l, rem))
	}
	return snum
}

func parseSnumLine(line string) (*Snum, string) {
	//	fmt.Printf("line: %s\n", line)
	snum := &Snum{}
	if line[0] == '[' {
		snum.l, line = parseSnumLine(line[1:])

		if line[0] != ',' {
			panic("No middle comma")
		}
		line = line[1:]

		snum.r, line = parseSnumLine(line)

		if line[0] != ']' {
			panic("No end square bracket")
		}
		line = line[1:]

		snum.l.up = snum
		snum.r.up = snum

		return snum, line
	}

	if !unicode.IsDigit(rune(line[0])) {
		panic(fmt.Sprintf("Non-digit at start of: %s", line))
	}

	snum.isInt = true
	end := strings.IndexFunc(line, func(r rune) bool { return !unicode.IsDigit(r) })
	if end < 0 {
		panic(fmt.Sprintf("trailing digits?: %s", line))
	}
	var err error
	snum.n, err = strconv.Atoi(line[:end])
	if err != nil {
		panic(fmt.Sprintf("Failed to atoi [%s]: %s", line[:end], err))
	}
	line = line[end:]

	return snum, line
}
