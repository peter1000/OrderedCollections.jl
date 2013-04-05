##############
# OrderedDict
# 
# This is a Dict which retains the insertion order of elements.  New
# key-value pairs are appended.  If a key from a key-value pair
# already exists in the dictionary, the original value is updated, and
# the key remains in its original position.
#
# In addition, many of the Dequeue-related functions are available:
#
#   push!(od, (k,v))      # Adds (k,v) to the end of the dictionary
#   pop!(od)              # Removes and returns the last key-value pair
#   unshift!(od, (k,v))   # Adds (k,v) to the front of the dictionary
#   shift!(od)            # Removes and returns the first key-value pair
#   insert!(od, i, (k,v)) # Inserts (k,v) at position i
#   append!(od, items)    # Adds (k,v) pairs from items to the end of
#                         # the dictionary
#
# Note also that this is not a sorted dictionary, although it can be
# sorted with 
#
#   sort!(od)              # od is an OrderedDict()
#   sortby!(od, x->od[x])  # sort by value
#   od2 = sort(od)         # od is not modified
#
# You can also sort normal dictionaries, and get a sorted OrderedDict
# back:
#
#   od = sort(d)         # d is a Dict; returns a sorted OrderedDict
#   #sort!(d)            # error! Dicts can't be sorted in place!
#
# Additional AbstractArray-like features
#
#   indexof(od, key)     # returns the index of key according to the current order
#   getitem(od, 2)       # returns the second (k,v) pair according to the current order
#   od[2]                # same, but only works for OrderedDicts where the
#                        # keys are not Numbers
#   first(od) == od[1]   
#   last(od) == od[end]
#   reverse!(od)         # reverses od in-place
#   reverse(od)          # creates a reversed copy of od

# Construction
import Base.similar, Base.sizehint

# Serialization
import Base.serialize, Base.deserialize

# Iteration
import Base.start, Base.next, Base.done

# General Collections
import Base.isempty, Base.empty!, Base.length

# Indexable Collections
import Base.setindex!, Base.getindex, Base.first, Base.last, Base.endof, Base.reverse, Base.reverse!,
       Base.findfirst, Base.findnext

# Associative Collections
import Base.has, Base.get, Base.getkey, Base.delete!

# Dequeue-like
import Base.push!, Base.pop!, Base.unshift!, Base.shift!, Base.append!, Base.insert!

# Useful, unexported by Base
import Base.findnextnot, Base.findfirstnot   ## from bitarray.jl
import Base._tablesz, Base.hashindex         ## from dict.jl
import Base.serialize_type

# Sorting

import Base.sort!, Base.sort, Base.sortby!, Base.sortby, Base.sortperm
import Sort.DEFAULT_UNSTABLE, Sort.DEFAULT_STABLE,
       Sort.Ordering, Sort.Algorithm, 
       Sort.Forward, Sort.Reverse,
       Sort.By, Sort.Lt, Sort.lt

# Exports

export OrderedDict,
    similar,
    sizehint,
    start,
    next,
    done,
    empty!,
    getindex,
    setindex!,
    push!,
    pop!,
    unshift!,
    shift!,
    append!,
    insert!,
    sort,
    sort!,
    sortby,
    sortby!,
    sortperm,
    indexof,
    getitem, 
    first,
    last,
    endof,
    reverse,
    reverse!


###################
## DictItem type ##

type DictItem{K,V} <: (K,V)
    k::K
    v::V
    idx::Int
end

convert{K,V}(::(K,V), d::DictItem{Any, Any}) = (convert(K,d.k), convert(V,d.v))

#######################
## DictOrdering type ##

type DictOrdering{K,V} <: AbstractVector{(K,V)}
    items::AbstractVector{DictItem{K,V}}
end

push!{K,V}(h::DictOrdering{K,V}, item::DictItem{K,V}) = push!(h.items, item)
length(h::DictOrdering) = length(h.ht)

###############
## Iteration ##

start(h::DictOrdering) = start(h.items)
done(h::DictOrdering, i) = done(h.items, i)
next(h::DictOrdering, i) = ((item, state) = next(h, i); ((item.k, item.v), state))

#########################
## General Collections ##

empty!(h::DictOrdering) = empty!(h.items)

###########################
## Indexable Collections ##
getindex{K,V}(h::DictOrdering{K,V}, idx::Real) = getindex(h.items, idx)::(K,V)
getindex{K,V}(h::DictOrdering{K,V}, r::Range) = [d::(K,V) for d in getindex(h.items, r)]

setindex!{K,V}(h::DictOrdering{K,V}, ) = setindex!(h.items, d, idx)
function setindex!{K,V}(h::DictOrdering{K,V}, kv::(Any,Any), index::Real)
    (key,v) = kv
    if indexof(h,key,0) == index
        h.items[index].k = v
        return kv::(K,V)
    end
    # TODO: this can made be more efficient
    delete!(h.ht, h.items[index].k)
    insert!(h, index, kv)
end

function reverse!(h::DictOrdering)
    _compact!(h)
    reverse!(h.items)
    _update_order(h, 1, length(h))
    h.ht # return the actual dictionary
end

function reverse(h::DictOrdering)
    d = similar(h.ht)
    sizehint(d, length(h.ht)<<1)
    _compact!(h)
    for item in reverse(h.items)
        d[item.k] = item.v
    end
    d
end

# Internal: do not compact, return DictItem(k,v,idx)
function _delete!{K,V}(h::DictOrdering{K,V}, item_idx::Real)
    item = h.items[item_idx]
    ccall(:jl_arrayunset, Void, (Any, Uint), h.items, item_idx-1)
    h.slots[item_idx] = false
    h.ndel += 1
    item
end

# External: compact, return (k, v)
function delete!{K,V}(h::DictOrdering{K,V}, index::Real)
    _compact!(h)
    item = _delete!(h, index)
    _delete!(h.ht, item.k)
    (item.k, item.v)::(K,V)
end

##################
## Dequeue-like ##

# Add key-value pair at last slot
push!{K,V}(d::DictOrdering{K,V}, item) = insert!(d, length(d)+1, item)

# Remove and return last key-value pair
function pop!{K,V}(d::DictOrdering{K,V})
    if isempty(d)
        error("pop!: DictOrdering is empty")
    end
    _compact!(d)
    item = _delete!(d, length(d))
    _delete!(d.ht, item.k)
    (item.k, item.v)::(K,V)
end

# Put key-value pair at front of dict
unshift!{K,V}(d::DictOrdering{K,V}, item) = insert!(d, 1, item)

# Remove and return first key-value pair
function shift!{K,V}(d::DictOrdering{K,V})
    if isempty(d)
        error("shift!: ",typeof(d.ht)," is empty")
    end
    idx = findfirst(d.slots)
    item = _delete!(d, idx)
    _delete!(d.ht, item.k)
    (item.k, item.v)::(K,V)
end

# TODO: prepend

# Add multiple items to dictionary, at end
function append!{K,V}(h::DictOrdering{K,V}, items)
    for item in items
        push!(h, item)
    end
    items
end

# Add item to dictionary at a particular linear location
# Note that if the key already exists in the dictionary, it is removed
# first, and this might decrement the inserted position by one
function insert!{K,V}(h::DictOrdering{K,V}, index::Integer, item::(Any,Any))
    (key,v) = item
    # Do we need to subtract 1 from index?
    cur_index = indexof(h, key, 0)
    if cur_index > 0 && cur_index < index
        index -= 1
    end
    # Add/set the element
    h.ht[key] = v

    # Shift the element, if necessary
    cur_index = indexof(h, key)          # calls _compact!()
    if cur_index == index
        return item::(K,V)
    end
    # _compact!(h)  ## called in indexof() above...
    _move_item!(h.items, cur_index, index)
    _update_order(h, index, cur_index)
    item::(K,V)
end

####################################
## DictOrdering Utility functions ##

indexof{K,V}(h::DictOrdering{K,V}, key)        = (_compact!(h); _getitem(h.ht, key).idx)
indexof{K,V}(h::DictOrdering{K,V}, key, deflt) = has(h.ht, key) ? indexof(h, key) : deflt

# Removes empty slots of order array in OrderedDict
function _compact!(h::DictOrdering)
    h.ndel == 0 && return

    items = h.items
    slots = h.slots

    # start with first empty slot
    s_pos = findfirstnot(slots)
    i_pos = findnext(slots, s_pos)
    
    # fill down empty slots with consecutive filled slots
    while i_pos != 0
        item = items[s_pos] = items[i_pos]
        item.idx = s_pos
        
        if i_pos == endof(items) break end

        s_pos += 1
        i_pos = findnext(slots, i_pos+1)
    end
    
    new_sz = length(h)
    resize!(h.items, new_sz)
    resize!(h.slots, new_sz)
    slots[1:end] = true
    h.ndel = 0

    nothing
end

function _update_order(h::DictOrdering, first, last)
    if first > last
        (first, last) = (last, first)
    end

    for i = first:last
        h.items[i].idx = i
    end
end

# move item in a from a[from] to a[to], shifting elements as needed
function _move_item!{T}(a::AbstractArray{T}, from, to)
    item = a[from]
    if from < to
        for i = from:to-1
            a[i] = a[i+1]
        end
    elseif from > to
        for i = from:-1:to+1
            a[i] = a[i-1]
        end
    end
    a[to] = item
    nothing
end

# Sorting
import Sort.Ordering, Sort.Algorithm

function sort!(h::DictOrdering, args...)
    _compact!(h)
    p = sortperm(keys(h.ht), args...)
    h.items[:] = h.items[p]
    _update_order(h, 1, length(h))
    h.ht
end

# Force stable sort by default, even for numerical keys
sort!{K<:Number,V}(h::DictOrdering{K,V}, o::Ordering) = sort!(h, DEFAULT_STABLE, o)
sort {K<:Number,V}(h::DictOrdering{K,V}, o::Ordering) = sort (h, DEFAULT_STABLE, o)

sortperm(h::DictOrdering, args...) = sortperm(keys(h.ht), args...)

######################
## OrderedDict type ##

type OrderedDict{K,V} <: Associative{K,V}
    ht::Dict{K,DictItem{K,V}}
    order::DictOrdering{K,V,OrderedDict{K,V}}

    function OrderedDict()
        d = new()
        d.ht = Dict{K,DictItem{K,V}}()
        d.order = DictOrdering{K,V,OrderedDict{K,V}}(similar(Array(DictItem{K,V},1), 0), BitArray(), 0, d)
        d
    end
    function OrderedDict(ks,vs)
        d = OrderedDict{K,V}()
        for (i, (k, v)) in enumerate(zip(ks, vs))
            item = DictItem{K,V}(k,v,i)
            d.ht[k] = item
            _push!(d.order, item)
        end
        d
    end
end
OrderedDict() = OrderedDict{Any, Any}()
OrderedDict(ks, vs) = OrderedDict{Any,Any}(ks, vs)
OrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = OrderedDict{K,V}(ks, vs)

# syntax entry points
OrderedDict{K,V}(ks::(K...), vs::(V...)) = OrderedDict{K  ,V  }(ks, vs)
OrderedDict{K  }(ks::(K...), vs::Tuple ) = OrderedDict{K  ,Any}(ks, vs)
OrderedDict{V  }(ks::Tuple , vs::(V...)) = OrderedDict{Any,V  }(ks, vs)
OrderedDict{K,V}(kvs::AbstractArray{(K,V)}) = OrderedDict{K,V}(zip(kvs...)...)
OrderedDict{K,V}(kvs::AbstractArray{DictItem{K,V}}) = OrderedDict{K,V}(zip(kvs...)...)
#OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict{K,V}(collect(d))  ## Why doesn't this work?
OrderedDict{K,V}(d::Associative{K,V}) = OrderedDict(collect(d))

##########################
## Construction-related ##
similar{K,V}(d::OrderedDict{K,V}) = OrderedDict{K,V}()
sizehint(d::OrderedDict, newsz) = sizehint(d.ht, newsz)

###################
## Serialization ##

function serialize(s, t::OrderedDict)
    serialize_type(s, typeof(t))
    write(s, int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function deserialize{K,V}(s, T::Type{OrderedDict{K,V}})
    n = read(s, Int32)
    t = T(); sizehint(t, n)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

###############
## Iteration ##

start(d::OrderedDict) = start(d.order)
done(d::OrderedDict, i) = done(d.order, i)
next(d::OrderedDict, i) = next(d.order, i)

#########################
## General Collections ##

isempty(d::OrderedDict) = isempty(d.ht)
length(d::OrderedDict) = length(d.ht)
empty!(d::OrderedDict) = (empty!(d.ht); empty!(d.order); d)

###########################
## Indexable Collections ##

getindex{K,V}(h::OrderedDict{K,V}, key) = getindex(h.ht, key).v

function setindex!{K,V}(d::OrderedDict{K,V}, v, key)
    # 3/4 deleted?
    if d.order.ndel >= ((3*length(d.order))>>2)
        _compact!(d.order)
    end

    if has(d, key)
        d.ht[key].v = v
    else
        item = DictItem{K,V}(key, v, length(d.order)+1)
        d.ht[key] = item
        _push!(d.order, item)
    end
    d
end

# We want to allow the user to access the (k,v) pairs by index
# However, first and foremost, this is a dictionary, so if the
# keys are numbers, assume any reference using an integer
# as a key is attempting a has lookup.

# Note: This might be confusing behavior, so disabled for now.
#       Use h.order[idx] to access (K,V) pairs by index

# function setindex!{K,V}(h::OrderedDict{K,V}, kv::(Any,Any), index::Integer)
#     (key,v) = kv
#     ord_idx = indexof(h,key,0)
#     if ord_idx == index
#         return setindex!(h, v, key)
#     end
#     # TODO: this can made be more efficient
#     delete!(h, getitem(h, index)[1])
#     insert!(h, index, kv)
# end

# setindex!{K<:Number,V}(h::OrderedDict{K,V}, v::(Any,Any), key::Integer) = 
#     invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)
# setindex!{K<:Number,V}(h::OrderedDict{K,V}, v, key::Integer) = 
#     invoke(setindex!, (OrderedDict{K,V}, Any, Any), h, v, key)

#getindex{K,        V}(h::OrderedDict{K,V}, ord_idx::Integer) = getitem(h.order, ord_idx)
#getindex{K<:Number,V}(h::OrderedDict{K,V}, key::Integer)     = getindex(h.ht, key).v

indexof{K,V}(h::OrderedDict{K,V}, args...) = indexof(h.order, args...)
findfirst(h::OrderedDict, k) = indexof(h, k, 0)
findnext(h::OrderedDict, k, start::Int) = (idx=indexof(h,k,0); idx >= start? idx : 0)

first(h::OrderedDict) = h.order[1]
last(h::OrderedDict) = h.order[length(h)]
endof(h::OrderedDict) = length(h)

reverse!(h::OrderedDict) = reverse!(h.order)
reverse(h::OrderedDict) = reverse(h.order)


#############################
## Associative Collections ##

has{K,V}(d::OrderedDict{K,V}, key) = has(d.ht, key)

get{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? d[key] : default
getkey{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? key : default
_getitem{K,V}(d::OrderedDict{K,V}, key) = d.ht[key]
_getitem{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? d.ht[key] : default

_delete!{K,V}(d::OrderedDict{K,V}, key) = delete!(d.ht, key)

function delete!{K,V}(d::OrderedDict{K,V}, key)
    item = delete!(d.ht, key)
    _delete!(d.order, item.idx)
    item.v
end

delete!{K,V}(d::OrderedDict{K,V}, key, default) = has(d, key) ? delete!(d, key) : default

#delete!{K,V}(h::OrderedDict{K,V}, ord_idx::Integer) = delete!{K,V}(h.order, ord_idx)
#delete!{K<:Number,V}(h::OrderedDict{K,V}, key::Integer) = invoke(delete!, (OrderedDict{K,V}, Any), h, key)

#############################
## Delegations...

push!(d::OrderedDict, kv) = push!(d.order, kv)
pop!(d::OrderedDict) = pop!(d.order)
unshift!(d::OrderedDict, kv) = unshift!(d.order, kv)
shift!(d::OrderedDict) = shift!(d.order)
insert!(d::OrderedDict, i::Integer, kv) = insert!(d.order, i, kv)
append!(d::OrderedDict, kvs) = append!(d.order, kvs)
#prepend!(d::OrderedDict, kvs) = prepend!(d.order, kvs)

#############
## Sorting ##

sort!(d::OrderedDict, args...) = sort!(d.order, args...)
sort(d::OrderedDict, args...) = sort!(d.order, args...)

sortby!(d::OrderedDict, args...) = sortby!(d.order, args...)
sortby(d::OrderedDict, args...) = sortby(d.order, args...)

sortperm(d::OrderedDict, args...) = sortperm(d.order, args...)
sortpermby(d::OrderedDict, args...) = sortpermby(d.order, args...)

function sort{K,V}(h::Dict{K,V}, args...)
    d = OrderedDict{K,V}()
    sizehint(d, length(h)<<1)
    for k in sort(keys(h), args...)
        d[k] = h[k]
    end
    d
end
