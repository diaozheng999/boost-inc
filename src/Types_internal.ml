type timestamp = {
  at: float;
  sub: float;
  mutable isSplicedOut: bool;
}

type time = timestamp Yalib.Linked_list.linked_list_node

type window = time * time

type 'a entry = ('a * window option) option ref

type unique = Yalib.Unique.t

type 'a box = { label: unique; value: 'a }
