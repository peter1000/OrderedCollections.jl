
importall Base
export OrderedDict,
    similar,
    isless,
    #serialize,
    #deserialize,
    rehash,
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
    length

##################
# Abstractions

abstract OrderedItem
    ## Members:
    #    idx::Int
abstract AbstractDictOrdering
abstract AbstractOrderedDict{K,V,Ord<:AbstractDictOrdering} <: Associative{K,V}
    ## Members:
    #    ht::Dict{K,OrderedDictItem{K,V}}

#################
# Ordered Items #

indexof(item::OrderedItem) = item.idx

#######################
# AbstractOrderedDict #

# Delegate functions
sizehint(d::AbstractOrderedDict, newsz) = sizehint(d.ht, newsz)
getindex{K,V}(h::AbstractOrderedDict{K,V}, key) = getindex(h.ht, key).value

has(h::AbstractOrderedDict, key) = has(h.ht, key)
getkey{K,V}(h::AbstractOrderedDict{K,V}, key, deflt) = getkey(h.ht, key, deflt)
getitem{K,V}(h::AbstractOrderedDict{K,V}, key)        = getindex(h.ht, key)
getitem{K,V}(h::AbstractOrderedDict{K,V}, key, deflt) = get(h.ht, key, deflt)

#get{K,V}(h::AbstractOrderedDict{K,V}, key, deflt) = has(h.ht, key) ? h.ht[key] : deflt
## TODO: decide whether to use the version of get above or below
##       above: two hash lookups
##       below: one hash lookup, but depends on structure of Dict
function get{K,V}(h::AbstractOrderedDict{K,V}, key, deflt)
    index = Base.ht_keyindex(h.ht, key)
    return (index<0) ? deflt : h.ht.vals[index].v::V
end

isempty(t::AbstractOrderedDict) = isempty(t.ht)
length(t::AbstractOrderedDict) = length(t.ht) # == t.ht.count

_empty!{K,V}(h::AbstractOrderedDict{K,V}) = empty!(h.ht)

###################
# OrderedDictItem #

type OrderedDictItem{K,V} <: OrderedItem
    key::K
    value::V
    idx::Int
end

isless(a::OrderedDictItem, b::OrderedDictItem) = isless(a.key, b.key)
show(io::IO, a::OrderedDictItem) = print(io, string(a.key,"=>",a.value))

# Iteration, to allow destructuring
start(a::OrderedDictItem) = 1
done(a::OrderedDictItem, i) = (i > 2)
next(a::OrderedDictItem, i) = (i == 1 ? a.key : a.value, i+1)

################
# DictOrdering #

type SimpleDictOrdering{K,V} <: AbstractDictOrdering
    v::Vector{OrderedDictItem{K,V}}
    dict::AbstractOrderedDict{K,V}

    SimpleDictOrdering(d::AbstractOrderedDict{K,V}) = new(Array(OrderedDictItem{K,V},0), d)
end

# Iteration
start(o::SimpleDictOrdering) = start(o.v)
done(o::SimpleDictOrdering,i) = done(o.v,i)
next(o::SimpleDictOrdering,i) = next(o.v,i)

isempty(o::SimpleDictOrdering) = isempty(o.dict)
_empty!(o::SimpleDictOrdering) = empty!(o.v)
empty!(o::SimpleDictOrdering) = (empty!(o.v); _empty!(o.dict); o)

size(o::SimpleDictOrdering) = size(o.v)
length(o::SimpleDictOrdering) = length(o.v)
endof(o::SimpleDictOrdering) = length(o.v)

contains(o::SimpleDictOrdering, x) = contains(o.v, x)
#findin(a,b) = findin(a.v, b) # TODO: what should this do
unique(o::SimpleDictOrdering) = o # TODO: copy?
reduce(o::SimpleDictOrdering, v0, op) = reduce(o.v, v0, op)
max(o::SimpleDictOrdering) = max(o.v)
min(o::SimpleDictOrdering) = min(o.v)

delete!(o::SimpleDictOrdering, idx::Real) = (item = delete!(o.v, idx); _delete!(o.dict, item.key); item)

getindex(a::SimpleDictOrdering) = getindex(a.v)
#setindex!(a::SimpleDictOrdering, args...) = setindex!(a, args...)

# TODO: add Dequeue operations
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


###############
# OrderedDict #

# SimpleOrderedDict

type SimpleOrderedDict{K,V} <: AbstractOrderedDict{K,V,SimpleDictOrdering}
    ht::Dict{K,OrderedDictItem{K,V}}
    order::SimpleDictOrdering{K,V}

    SimpleOrderedDict() = (x = new(); x.ht = Dict{K,OrderedDictItem{K,V}}(); x.order = SimpleDictOrdering{K,V}(x))
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

# Utility functions

function _delete!(ord::Vector{Ordered}, i::Integer)
    delete!(ord, i)
    for idx = i:endof(ord)
        ord[i].idx = i
    end
end

function _delete!{T<:Integer}(ord::Vector{Ordered}, r::Range{T})
    delete!(ord, r)
    for idx = first(r).:endof(ord)
        ord[i].idx = i
    end
end


# Dict functions

empty!{K,V}(h::SimpleOrderedDict{K,V}) = (empty!(h.ht); _empty!(h.order); h)

function setindex!{K,V}(h::SimpleOrderedDict{K,V}, v, key)
    key = convert(K, key)
    v   = convert(V, v)

    if has(h.ht, key)
        h.ht[key].value = v
        return v
    end
    item = OrderedDictItem{K,V}(convert(K,key), convert(V, v), length(h.order)+1)
    h.ht[key] = item
    push!(h.order, item)
    v
end

_delete!(h::SimpleOrderedDict, key) = delete!(h.ht, key)

function delete!(h::SimpleOrderedDict, key)
    item = delete!(h.ht, key)
    _delete!(h.order, item.idx)
    item.value
end

#delete!(h::SimpleOrderedDict, key, default) = has(h.ht, key) ? delete!(h, key) : default
## TODO: decide whether to use the version of delete! above or below
##       above: two hash lookups
##       below: one hash lookup, but depends on structure of Dict
function delete!(h::SimpleOrderedDict, key, default)
    index = Base.ht_keyindex(h.ht, key)
    if index > 0 
        item = Base._delete!(h.ht, index)
        _delete!(h.order, item.idx)
        item.value
    else
        default
    end
end

start(t::SimpleOrderedDict) = start(t.order)
done(t::SimpleOrderedDict, i) = done(t.order, i)
next(t::SimpleOrderedDict, i) = next(t.order, i)


