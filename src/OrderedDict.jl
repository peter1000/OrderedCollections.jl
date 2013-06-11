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
import Base.haskey, Base.get, Base.getkey, Base.delete!, Base.splice!

# Dequeue-like
import Base.push!, Base.pop!, Base.unshift!, Base.shift!, Base.append!, Base.insert!

# Sorting

# import Base.sort!, Base.sort, Base.sortby!, Base.sortby, Base.sortperm
# import Sort.DEFAULT_UNSTABLE, Sort.DEFAULT_STABLE,
#        Sort.Ordering, Sort.Algorithm, 
#        Sort.Forward, Sort.Reverse,
#        Sort.By, Sort.Lt, Sort.lt

# Exports

export 
    OrderedDictBase,
    OrderedDict,
    ArrayOrderedDict,
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

# by JMW
macro delegate(source, targets)
    typename = esc(source.args[1])
    fieldname = esc(Expr(:quote, source.args[2].args[1]))
    funcnames = targets.args
    n = length(funcnames)
    fdefs = Array(Any, n)
    for i in 1:n
        funcname = esc(funcnames[i])
        fdefs[i] = quote
                     ($funcname)(a::($typename), args...) =
                       ($funcname)(a.($fieldname), args...)
                   end
    end
    return Expr(:block, fdefs...)
end
#

# jb (from graphics.jl)
macro mustimplement(sig)
    fname = sig.args[1]
    arg1 = sig.args[2]
    if isa(arg1,Expr)
        arg1 = arg1.args[1]
    end
    :($(esc(sig)) = error(typeof($(esc(arg1))),
                          " must implement ", $(Expr(:quote,fname))))
end

#####################
## OrderedDictBase ##

abstract DictItem{K,V}

type OrderedDictBase{K,V,Item<:DictItem,Itr} <: Associative{K,V}
    ht::Dict{K,Item} # really: Item{K,V}
    order::Itr       # really: Itr{Item{K,V}}

    OrderedDictBase() = OrderedDictBase(K,V,Item,Itr)  # type callback
    OrderedDictBase(ht::Dict{K,Item}, order::Itr) = new(ht, order)
end

function OrderedDictBase{K,V,Item<:DictItem,Itr}(ht::Dict{K,Item}, order::Itr, ::Type{V})
    OrderedDictBase{K,V,Item,Itr}(ht,order)
end

@delegate OrderedDictBase.ht    [sizehint isempty length haskey getkey]
@delegate OrderedDictBase.order [start done next]

# The following must be implemented for each DictItem/Iterable combination
@mustimplement getindex{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr}, key)
@mustimplement get{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr}, key, default)
@mustimplement setindex!{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr}, v, key)
@mustimplement delete!{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr}, key)
@mustimplement delete!{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr}, key, default)
@mustimplement empty!{K,V,Item<:DictItem,Itr}(d::OrderedDictBase{K,V,Item,Itr})

########################################
## Linked-list based OrderedDict type ##

type LinkedDictItem{K,V} <: DictItem{K,V}
    k::K
    v::V
    prev::LinkedDictItem
    next::LinkedDictItem

    # Constructor for empty item
    LinkedDictItem() = (item = new(); item.prev = item.next = item)
    LinkedDictItem(k, v) = (item = new(); item.k=convert(K,k); item.v=convert(V,v); item.prev = item.next = item)
end

function push!{K,V}(lst::LinkedDictItem{K,V}, item::LinkedDictItem{K,V})
    item.prev = lst.prev
    item.next = lst
    lst.prev.next = item
    lst.prev = item
end

function splice!{K,V}(lst::LinkedDictItem{K,V}, item::LinkedDictItem{K,V})
    item.prev.next = item.next
    item.next.prev = item.prev
end

function empty!{K,V}(lst::LinkedDictItem{K,V})
    item.prev = item.next = item
end

start{K,V}(lst::LinkedDictItem{K,V}) = lst.next
done{K,V}(lst::LinkedDictItem{K,V}, ptr::LinkedDictItem{K,V}) = is(ptr, lst)
next{K,V}(lst::LinkedDictItem{K,V}, ptr::LinkedDictItem{K,V}) = ((ptr.k, ptr.v), ptr.next)

######################################

typealias _OrderedDict{K,V} OrderedDictBase{K,V,LinkedDictItem,LinkedDictItem}

OrderedDict() = OrderedDict(Any,Any)
OrderedDict(K::Type, V::Type) = OrderedDictBase{K,V,LinkedDictItem,LinkedDictItem}()

function OrderedDictBase(K::Type, V::Type, ::Type{LinkedDictItem}, ::Type{LinkedDictItem})
    OrderedDictBase{K,V,LinkedDictItem,LinkedDictItem}(Dict{K,LinkedDictItem}(),LinkedDictItem{K,V}())
end

# required
getindex{K,V}(h::_OrderedDict{K,V}, key) = getindex(h.ht, key).v

# required
function get{K,V}(d::_OrderedDict{K,V}, key, default)
    item = get(d.ht, key, Base.secret_table_token)
    if is(item, Base.secret_table_token)
        return default
    end
    item.v
end

# required
function setindex!{K,V}(d::_OrderedDict{K,V}, v, key)
    if haskey(d, key)
        d.ht[key].v = v
    else
        item = LinkedDictItem{K,V}(key, v)
        d.ht[key] = item
        push!(d.order, item)
    end
    d
end

# required
function delete!{K,V}(d::_OrderedDict{K,V}, key)
    item = delete!(d.ht, key)
    splice!(d.order, item)
    item.v
end

# required
function delete!{K,V}(d::_OrderedDict{K,V}, key, default)
    item = delete!(d.ht, key, Base.secret_table_token)
    if is(item, Base.secret_table_token)
        return default
    end
    splice!(d.order, item)
    item.v
end

# required
empty!{K,V}(d::_OrderedDict{K,V}) = (empty!(d.ht); empty!(d.order); d)


###################################
## Vector-based OrderedDict type ##

type ArrayDictItem{K,V} <: DictItem{K,V}
    k::K
    v::V
    idx::Integer  # Item location in array
end

start{K,V}(lst::ArrayDictItem{K,V}) = 1
done{K,V}(lst::ArrayDictItem{K,V}, i) = i>2
next{K,V}(lst::ArrayDictItem{K,V}, i) = (i==1?lst.k:lst.v, i+1)

#

typealias _ArrayOrderedDict{K,V} OrderedDictBase{K,V,ArrayDictItem,Array}

ArrayOrderedDict() = ArrayOrderedDict(Any,Any)
ArrayOrderedDict(K::Type, V::Type) = OrderedDictBase{K,V,ArrayDictItem,Array}()

function OrderedDictBase(K::Type, V::Type, ::Type{ArrayDictItem}, ::Type{Array})
    OrderedDictBase{K,V,ArrayDictItem,Array}(Dict{K,ArrayDictItem}(),ArrayDictItem[])
end


# Utility function to fix item order
function update_item_order(h::_ArrayOrderedDict, first::Int, last::Int)
    if first > last
        (first, last) = (last, first)
    end

    for i = first:last
        h.order[i].idx = i
    end

    nothing
end
update_item_order(h::_ArrayOrderedDict) = update_item_order(h, 1, length(h.order))

# required
getindex{K,V}(h::_ArrayOrderedDict{K,V}, key) = getindex(h.ht, key).v

# required
function get{K,V}(d::_ArrayOrderedDict{K,V}, key, default)
    item = get(d.ht, key, Base.secret_table_token)
    if is(item, Base.secret_table_token)
        return default
    end
    item.v
end
    

# required
function setindex!{K,V}(d::_ArrayOrderedDict{K,V}, v, key)
    if haskey(d, key)
        d.ht[key].v = v
    else
        item = ArrayDictItem{K,V}(key, v, length(d.order)+1)
        d.ht[key] = item
        push!(d.order, item)
    end
    d
end

# required
function delete!{K,V}(d::_ArrayOrderedDict{K,V}, key)
    item = delete!(d.ht, key)
    splice!(d.order, item.idx)
    if item.idx <= length(d.order)
        update_item_order(d, item.idx, length(d.order))
    end
    item.v
end

# required
function delete!{K,V}(d::_ArrayOrderedDict{K,V}, key, default)
    item = delete!(d.ht, key, Base.secret_table_token)
    if is(item, Base.secret_table_token)
        return default
    end
    splice!(d.order, item.idx)
    if item.idx <= length(d.order)
        update_item_order(d, item.idx, length(d.order))
    end
    item.v
end

# required
empty!(d::_ArrayOrderedDict) = (empty!(d.ht); empty!(d.order); d)

