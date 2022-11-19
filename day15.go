package aoc

import (
	"fmt"
	"io"
)

type Day15 struct{}

func NewDay15() *Day15 {
	d := Day15{}
	return &d
}

func (d *Day15) Run(out io.Writer, lines []string) error {
	fmt.Fprintf(out, "Running\n")
	return nil
}
