
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

# Enumerable type is required to generalize the enumerabilty of items held in an OrderedDict

abstract Enumerable 
    ## Assumed to contain:
    # idx

indexof(item::Enumerable) = item.idx

# OrderedDictItem

type OrderedDictItem{K,V} <: Enumerable
    key::K
    value::V
    idx::Int
end

isless(a::OrderedDictItem, b::OrderedDictItem) = isless(a.key, b.key)


# OrderedDict

AbstractOrderedDict{K,V,Ord<:AbstractVector} <: Associative{K,V}
## Assumed to contain
#    ht::Dict{K,OrderedDictItem{K,V}}

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

# SimpleOrderedDict

type SimpleOrderedDict{K,V} <: AbstractOrderedDict{K,V,Vector}
    ht::Dict{K,OrderedDictItem{K,V}}
    order::Vector{OrderedDictItem{K,V}}

    SimpleOrderedDict() = new(Dict{K,OrderedDictItem{K,V}}(), Array(OrderedDictItem{K,V},0))
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

function _delete!(ord::Vector{Enumerable}, i::Integer)
    delete!(ord, i)
    for idx = i:endof(ord)
        ord[i].idx = i
    end
end

function _delete!{T<:Integer}(ord::Vector{Enumerable}, r::Range{T})
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
next(t::SimpleOrderedDict, i) = ((item, n) = next(t.order, i); ((item.key, item.value), n))


abstract AbstractDictOrdering <: AbstractArray

type DO{K,V,OD<:AbstractOrderedDict} <: AbstractDictOrdering
    v::Vector{OrderedDictItem{K,V}}
    dict::OD
end

# Iteration
start(o::DO) = start(o.v)
done(o::DO,i) = done(o.v, i)
next(o::DO,i) = ((item, ii) = next(o.v,i); ((item.key, item.value), ii))

isempty(o::DO) = isempty(d)
_empty!(o::DO) = empty(o.v)
empty!(o::DO) = (empty!(o.v); _empty!(o.d); o)

size(a::DO) = size(a.v)
length(a::DO) = length(a.v)
endof(a::DO) = length(a.v)

contains(o::DO, x) = contains(o.v, x)
findin(a,b) = findin(a.v, b)
unique(o::DO) = o # TODO: copy?
reduce(o::DO, v0, op) = reduce(o.v, v0, op)
max(o::DO) = max(o.v)
min(o::DO) = min(o.v)

delete!(o::DO, idx::Real) = (item = delete!(o.v, idx); _delete!(o.d, item.key); (item.key, item.value))
#TODO: add Dequeue operations
#push!
#pop!
#unshift!
#shift!
#prepend!
#append!
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



getindex(a::DO) = getindex(a.v)
setindex!(a::DO, args...) = setindex!(a, args...)
