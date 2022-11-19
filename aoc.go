package aoc

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"strings"
)

type Day interface {
	Run(out io.Writer, lines []string) error
}

func Run(day int, test bool, out io.Writer) error {
	lines := GetLines(day, test)
	fmt.Fprintf(out, "Lines are %v\n", lines)

	d, err := intToDay(day)
	if err != nil {
		return nil
	}
	err = d.Run(out, lines)
	if err != nil {
		return fmt.Errorf("Failed running day [%d]: $s", err)
	}

	return nil
}

func intToDay(day int) (Day, error) {
	var d Day

	switch day {
	case 12:
		d = NewDay12()
	default:
		return nil, fmt.Errorf("Unknown day [%d]", day)
	}
	return d, nil
}

func GetLines(day int, test bool) []string {
	fname := dataFileName(day, test)
	buf, err := ioutil.ReadFile(fname)
	if err != nil {
		log.Fatalf("Can't read data file [%s]: %s", fname, err)
	}
	lines := strings.Split(string(buf), "\n")
	return Filter(func(s string) bool { return s != "" }, lines)
}

func dataFileName(day int, test bool) string {
	workDir := "/home/john/dev/jbert/aoc2021"
	suffix := ""
	if test {
		suffix = "-test"
	}
	return fmt.Sprintf("%s/data/day%d%s.txt", workDir, day, suffix)
}

/*
 aoc-get-stream
 aoc-get-nums
 aoc-set-day
 aoc-set-test

 hash-key-add

 count-inc
 count-inc-foldl
 count-add

 list-nth
 list-partitionf

 half-cartesian-product
*/
