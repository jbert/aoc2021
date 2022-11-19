package aoc

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

type Set[T comparable] map[T]struct{}

func NewSet[T comparable]() Set[T] {
	return make(map[T]struct{})
}

func (s Set[T]) Contains(a T) bool {
	_, ok := s[a]
	return ok
}

func (s Set[T]) Intersect(a Set[T]) Set[T] {
	i := NewSet[T]()
	for x, _ := range s {
		if a.Contains(x) {
			i.Insert(x)
		}
	}
	return i
}

func (s Set[T]) Union(a Set[T]) Set[T] {
	u := NewSet[T]()
	for x, _ := range s {
		u.Insert(x)
	}
	for x, _ := range a {
		u.Insert(x)
	}
	return u
}

func SetFromList[T comparable](as []T) Set[T] {
	s := NewSet[T]()
	s.InsertList(as)
	return s
}

func (s Set[T]) Equals(a Set[T]) bool {
	if s.Size() != a.Size() {
		return false
	}
	for x, _ := range s {
		if !a.Contains(x) {
			return false
		}
	}
	return true
}

func (s Set[T]) InsertList(as []T) {
	for _, a := range as {
		s.Insert(a)
	}
}

func (s Set[T]) Insert(a T) {
	s[a] = struct{}{}
}

func (s Set[T]) Remove(a T) {
	delete(s, a)
}

func (s Set[T]) Size() int {
	return len(s)
}

func (s Set[T]) IsEmpty() bool {
	return s.Size() == 0
}

func (s Set[T]) ToList() []T {
	l := make([]T, s.Size())
	for k, _ := range s {
		l = append(l, k)
	}
	return l
}
