type timestamp = {
  at: float;
  sub: float;
  mutable isSplicedOut: bool;
}

type time = timestamp LinkedListImpl.boost_linked_list_node

type window = time * time

type 'a entry = ('a * window option) option ref

type unique

type 'a box = { label: unique; value: 'a }
