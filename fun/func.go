package fun

func Map[A any, B any](f func(A) B, as []A) []B {
	bs := make([]B, len(as))
	for i := range as {
		bs[i] = f(as[i])
	}
	return bs
}

func Filter[A any](pred func(A) bool, as []A) []A {
	fs := make([]A, 0)
	for _, a := range as {
		if pred(a) {
			fs = append(fs, a)
		}
	}
	return fs
}
