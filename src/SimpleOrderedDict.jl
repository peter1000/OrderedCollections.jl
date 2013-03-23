#using Debug
importall Base
export SimpleOrderedDict,
    OrderedDict,
    DictOrdering,
    similar,
    isless,
    #serialize,
    #deserialize,
    sizehint,
    empty!,
    setindex!,
    getindex,
    get,
    getkey,
    delete!,
    start,
    done,
    next,
    isempty,
    length,
    _delete!

##################
# Abstractions
#
# These three abstractions are the basic components of an OrderedDict
#
#     OrderedItem           An item which has a position in an ordering
#     AbstractDictOrdering  An ordering for a Dict
#     AbsractOrderedDict    A Dict which is ordered
#
#   In particular, an OrderedDictItem is a member of both a 
#   DictOrdering and an OrderedDict.
#

abstract OrderedItem
    ## Members:
    #    idx::Int
abstract AbstractDictOrdering{T} <: AbstractVector{T}
abstract AbstractOrderedDict{K,V,Ord<:AbstractDictOrdering} <: Associative{K,V}
    ## Members:
    #    ht::Dict{K,OrderedDictItem{K,V}}
    #    order::AbstractDictOrdering

#################
# Ordered Items #
#
# All ordered items must have an .idx member

indexof(item::OrderedItem) = item.idx

function update_indices!{O<:OrderedItem}(ord::AbstractVector{O}, first::Integer, last::Integer)
    for i = first:last
        ord[i].idx = i
    end
end

#######################
# AbstractOrderedDict #
#
# ht::Dict and order::AbstractDictOrdering must be members of all AbstractOrderedDicts.
# The methods below delegate much of the Dict functions to ht
#

# Delegate functions
sizehint(d::AbstractOrderedDict, newsz)               = sizehint(d.ht, newsz)
getindex{K,V}(h::AbstractOrderedDict{K,V}, key)       = getindex(h.ht, key).value

has(h::AbstractOrderedDict, key) = has(h.ht, key)
getkey{K,V}(h::AbstractOrderedDict{K,V}, key, deflt)  = getkey(h.ht, key, deflt)
getitem{K,V}(h::AbstractOrderedDict{K,V}, key)        = getindex(h.ht, key)
getitem{K,V}(h::AbstractOrderedDict{K,V}, key, deflt) = get(h.ht, key, deflt)

# TODO: add to Dict
hashvalue{K,V}(h::Dict{K,V}, index::Integer) = h.vals[index]

#get{K,V}(h::AbstractOrderedDict{K,V}, key, deflt) = has(h.ht, key) ? h.ht[key] : deflt
## TODO: decide whether to use the version of get above or below
##       above: two hash lookups
##       below: one hash lookup, but depends on hashvalue()
function get{K,V}(h::AbstractOrderedDict{K,V}, key, deflt)
   index = Base.ht_keyindex(h.ht, key)
   return (index<0) ? deflt : hashvalue(h.ht,index).value::V
end

isempty(t::AbstractOrderedDict) = isempty(t.ht)
length(t::AbstractOrderedDict) = length(t.ht) # == t.ht.count

_empty!{K,V}(h::AbstractOrderedDict{K,V}) = empty!(h.ht)

empty!{K,V}(h::AbstractOrderedDict{K,V}) = (empty!(h.ht); _empty!(h.order); h)

function setindex!{K,V}(h::AbstractOrderedDict{K,V}, v, key)
    key = convert(K, key)
    v   = convert(V, v)
    
    # current = get(h.ht, key, nothing)

    # if current != nothing
    #     current.value = v
    #     return v
    # end
    
    # # Avoid extra hash calculation
    index = Base.ht_keyindex(h.ht, key)
    if index > 0
        hashvalue(h.ht, index).value = v
        return v
    end

    item = OrderedDictItem{K,V}(key, v, endof(h.order)+1)
    h.ht[key] = item
    _push!(h.order, item)
    v
end

_delete!(h::AbstractOrderedDict, key) = delete!(h.ht, key)

function delete!(h::AbstractOrderedDict, key)
    item = delete!(h.ht, key)
    _delete!(h.order, item.idx)
    item.value
end

#delete!(h::AbstractOrderedDict, key, default) = has(h.ht, key) ? delete!(h, key) : default
## TODO: decide whether to use the version of delete! above or below
##       above: two hash lookups
##       below: one hash lookup, but depends on Base._delete!
function delete!(h::AbstractOrderedDict, key, default)
    index = Base.ht_keyindex(h.ht, key)
    if index > 0 
        item = Base._delete!(h.ht, index)
        _delete!(h.order, item.idx)
        item.value
    else
        default
    end
end

start(t::AbstractOrderedDict) = start(t.order)
done(t::AbstractOrderedDict, i) = done(t.order, i)
next(t::AbstractOrderedDict, i) = next(t.order, i)


###################
# OrderedDictItem #
#
# These are the individual items in OrderedDicts and DictOrderings
#

type OrderedDictItem{K,V} <: OrderedItem
    key::K
    value::V
    idx::Int
end

isless(a::OrderedDictItem, b::OrderedDictItem) = isless(a.key, b.key)
show(io::IO, a::OrderedDictItem) = (show(io, a.key); print(io, "=>"); show(io, a.value))

# Iteration, to allow destructuring
start(a::OrderedDictItem) = 1
done(a::OrderedDictItem, i) = (i > 2)
next(a::OrderedDictItem, i) = (i == 1 ? a.key : a.value, i+1)

######################
# SimpleDictOrdering #
#
# Defines an ordering for an OrderedDict.
#
# Not efficient: delete!() is O(N)
#

type SimpleDictOrdering{K,V} <: AbstractDictOrdering{OrderedDictItem{K,V}}
    dict::AbstractOrderedDict{K,V}
    items::Vector{OrderedDictItem{K,V}}

    SimpleDictOrdering(d::AbstractOrderedDict{K,V}) = new(d, Array(OrderedDictItem{K,V},0))
end

# Iteration
start(o::SimpleDictOrdering) = start(o.items)
done(o::SimpleDictOrdering,i) = done(o.items,i)
next(o::SimpleDictOrdering,i) = next(o.items,i)

isempty(o::SimpleDictOrdering) = isempty(o.dict)
_empty!(o::SimpleDictOrdering) = empty!(o.items)
empty!(o::SimpleDictOrdering) = (empty!(o.items); _empty!(o.dict); o)

size(o::SimpleDictOrdering) = size(o.items)
length(o::SimpleDictOrdering) = length(o.items)
endof(o::SimpleDictOrdering) = length(o.items)

contains(o::SimpleDictOrdering, x) = contains(o.items, x)
#findin(a,b) = findin(a.items, b) # TODO: what should this do
unique(o::SimpleDictOrdering) = o # TODO: copy?
reduce(o::SimpleDictOrdering, v0, op) = reduce(o.items, v0, op)
max(o::SimpleDictOrdering) = max(o.items)
min(o::SimpleDictOrdering) = min(o.items)

_delete!(o::SimpleDictOrdering, idx::Real) = (item = delete!(o.items, idx); update_indices!(o.items, idx, endof(o.items)); item)
delete!(o::SimpleDictOrdering, idx::Real) = (item = _delete!(o, idx); _delete!(o.dict, item.key); item)

getindex(a::SimpleDictOrdering) = getindex(a.items)
#setindex!(a::SimpleDictOrdering, args...) = setindex!(a, args...)

_push!(a::SimpleDictOrdering, args...) = push!(a.items, args...)

# TODO: add remaining Dequeue operations
#push!
#pop!
#unshift!
#shift!
#prepend!
#append!
#insert!
#eltype
#sizehint

# TODO: delegate dict operations which don't conflict?
#get
#getkey
#getitem
#keys
#values
#collect
#merge
#merge!
#filter
#filter!


#####################
# SimpleOrderedDict
#
# OrderedDict based on a SimpleDictOrdering
#

type SimpleOrderedDict{K,V} <: AbstractOrderedDict{K,V,SimpleDictOrdering}
    ht::Dict{K,OrderedDictItem{K,V}}
    order::SimpleDictOrdering{K,V}

    SimpleOrderedDict() = (x = new(); x.ht = Dict{K,OrderedDictItem{K,V}}(); x.order = SimpleDictOrdering{K,V}(x); x)
    function SimpleOrderedDict(ks, vs)
        n = length(ks)
        h = SimpleOrderedDict{K,V}()
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
end

SimpleOrderedDict() = SimpleOrderedDict{Any,Any}()

SimpleOrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = SimpleOrderedDict{K,V}(ks,vs)
SimpleOrderedDict(ks, vs) = SimpleOrderedDict{Any,Any}(ks, vs)

# syntax entry points
SimpleOrderedDict{K,V}(ks::(K...), vs::(V...)) = SimpleOrderedDict{K  ,V  }(ks, vs)
SimpleOrderedDict{K  }(ks::(K...), vs::Tuple ) = SimpleOrderedDict{K  ,Any}(ks, vs)
SimpleOrderedDict{V  }(ks::Tuple , vs::(V...)) = SimpleOrderedDict{Any,V  }(ks, vs)

similar{K,V}(d::SimpleOrderedDict{K,V}) = SimpleOrderedDict{K,V}()

############################################################################################

######################
# DictOrdering #
#
# Defines a more efficient ordering for an OrderedDict.
#

#######################
## DictOrdering type ##

type DictOrdering{K,V} <: AbstractDictOrdering{OrderedDictItem{K,V}}
    ht::AbstractOrderedDict{K,V}
    items::AbstractVector{OrderedDictItem{K,V}}
    #slots::BitArray
    slots::Vector{Bool}
    ndel::Int

    #DictOrdering(d::AbstractOrderedDict{K,V}) = new(d, Array(OrderedDictItem{K,V},0), BitVector(), 0)
    DictOrdering(d::AbstractOrderedDict{K,V}) = new(d, Array(OrderedDictItem{K,V},0), Array(Bool,0), 0)
end

function _push!{K,V}(h::DictOrdering{K,V}, item::OrderedDictItem{K,V})
    push!(h.slots, true)
    push!(h.items, item)
end

length(h::DictOrdering) = length(h.ht)
size(h::DictOrdering) = length(h.ht)
endof(h::DictOrdering) = length(h.items)


###############
## Iteration ##

#skip_deleted(h::DictOrdering, i) = findnext(h.slots, i)

start(h::DictOrdering) = length(h.slots) > 0 && findnext(h.slots, 1) # skip_deleted(h,1)
done(h::DictOrdering, i) = (i == 0)
next(h::DictOrdering, i) = (h.items[i], findnext(h.slots,i+1)) # skip_deleted(h,i+1)))

#########################
## General Collections ##

_empty!(h::DictOrdering) = (empty!(h.items); empty!(h.slots); h.ndel = 0; h)
empty!(h::DictOrdering) = (_empty!(h); _empty!(h.ht); h)

###########################
## Indexable Collections ##
function getindex{K,V}(h::DictOrdering{K,V}, idx::Real)
    _compact!(h)
    h.items[idx]::OrderedDictItem{K,V}
end

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
        d[item.key] = item.value
    end
    d
end

#@debug begin

# Internal: do not compact, return OrderedDictItem(k,v,idx)
function _delete!{K,V}(h::DictOrdering{K,V}, item_idx::Real)
    #@bp 
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
    _delete!(h.ht, item.key)
    item::OrderedDictItem{K,V}
end

#end #@debug


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
    _delete!(d.ht, item.key)
    item::OrderedDictItem{K,V}
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
    _delete!(d.ht, item.key)
    item::OrderedDictItem{K,V}
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
    #s_pos = findfirstnot(slots)
    s_pos = findfirst(x->!x, slots)
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
    #h.slots = trues(new_sz)
    h.slots = fill(true, new_sz)
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

#####################
# OrderedDict
#
# OrderedDict based on a DictOrdering
#

type OrderedDict{K,V} <: AbstractOrderedDict{K,V,DictOrdering}
    ht::Dict{K,OrderedDictItem{K,V}}
    order::DictOrdering{K,V}

    OrderedDict() = (x = new(); x.ht = Dict{K,OrderedDictItem{K,V}}(); x.order = DictOrdering{K,V}(x); x)
    function OrderedDict(ks, vs)
        n = length(ks)
        h = OrderedDict{K,V}()
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
end

OrderedDict() = OrderedDict{Any,Any}()

OrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = OrderedDict{K,V}(ks,vs)
OrderedDict(ks, vs) = OrderedDict{Any,Any}(ks, vs)

# syntax entry points
OrderedDict{K,V}(ks::(K...), vs::(V...)) = OrderedDict{K  ,V  }(ks, vs)
OrderedDict{K  }(ks::(K...), vs::Tuple ) = OrderedDict{K  ,Any}(ks, vs)
OrderedDict{V  }(ks::Tuple , vs::(V...)) = OrderedDict{Any,V  }(ks, vs)

similar{K,V}(d::OrderedDict{K,V}) = OrderedDict{K,V}()

