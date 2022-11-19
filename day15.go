package aoc

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Day15 struct{}

func NewDay15() *Day15 {
	d := Day15{}
	return &d
}

func (d *Day15) Run(out io.Writer, lines []string) error {
	fmt.Fprintf(out, "Running\n")
	grid := GridFromLines(lines)
	fmt.Printf("Grid:\n%v\n", grid)
	return nil
}

type Grid [][]int

func GridFromLines(lines []string) Grid {

	var grid [][]int
	for _, line := range lines {
		var row []int
		digits := strings.Split(line, "")
		for _, digit := range digits {
			num, err := strconv.Atoi(digit)
			if err != nil {
				panic(fmt.Sprintf("Bad digit? [%s]: %s", digit, err))
			}
			row = append(row, num)
		}
		grid = append(grid, row)
	}

	return grid
}
