OrderedCollections
==================

An ordered collection is simply a collection which has a particular
order.  This package follows the convention where the default order is
the insertion order of items in the collection, although this order
can be changed via dequeue-like operations or sorting. 

OrderedDict
===========
`OrderedDict` here is an `Associative` which retains the insertion
order of elements.  New key-value pairs are appended.  If a key from a
key-value pair already exists in the dictionary, the original value is
updated, and the key remains in its original position.

In addition, many `Dequeue`-related functions are available:

```julia
  push!(od, (k,v))     # Adds (k,v) to the end of the dictionary
  pop!(od)             # Removes and returns the last key-value pair
  unshift!(od, (k,v))  # Adds (k,v) to the front of the dictionary
  shift!(od)           # Removes and returns the first key-value pair
  append!(od, items)   # Adds (k,v) pairs from items to the end of
                       # the dictionary
```

Note also that this is not necessarily a sorted dictionary, although
it can be sorted with

```julia
  sort!(od)              # od is an OrderedDict()
  sortby!(od, x->od[x])  # sort by value
  od2 = sort(od)         # od is not modified
```

You can also sort normal dictionaries, and get a sorted `OrderedDict`
back:

```julia
  od = sort(d)         # d is a Dict; returns a sorted OrderedDict
  #sort!(d)            # error! Dicts can't be sorted in place!
```

Implementation
--------------
`OrderedDict` was implemented in a brute force manner: by copying and
modifying Julia's `base/dict.jl`.  Various other methods of wrapping
Dict were tried and discarded for performance reasons.

This version has about a 40% insertion and 10% deletion slowdown
compared with Dict().

OrderedSet
==========
(In progress...)
