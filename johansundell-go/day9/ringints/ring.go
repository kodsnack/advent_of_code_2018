package ringints

type RingInt struct {
	Id   int
	Prev *RingInt
	Next *RingInt
}

func New() *RingInt {
	s := &RingInt{}
	s.Id = 0
	s.Next = s
	s.Prev = s
	return s
}

func (s *RingInt) Insert(stone *RingInt) *RingInt {
	next := s.Next
	s.Next = stone
	stone.Prev = s
	stone.Next = next
	next.Prev = stone
	return stone
}

func (s *RingInt) RemoveNext() *RingInt {
	next := s.Next
	s.Next = next.Next
	s.Next.Prev = s
	return next
}
