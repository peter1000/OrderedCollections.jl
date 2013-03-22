module OrderedDict2

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

function fixidx{K,V}(ord::AbstractVector{Enumerable}, start::Int, stop::Int)
    for i = start:stop
        ord[i].idx = i
    end
end

# Dict functions

empty!{K,V}(h::SimpleOrderedDict{K,V}) = (empty!(h.ht); empty!(h.order); h)

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

function delete!(h::SimpleOrderedDict, key)
    item = delete!(h.ht, key)
    delete!(h.order, item.idx)
    fixidx(h.order, item.idx, endof(h.order))
    item.value
end

#delete!(h::SimpleOrderedDict, key, default) = has(h.ht, key) ? delete!(h, key) : default
## TODO: decide whether to use the version of get above or below
##       above: two hash lookups
##       below: one hash lookup, but depends on structure of Dict
function delete!(h::SimpleOrderedDict, key, default)
    index = Base.ht_keyindex(h.ht, key)
    if index > 0 
        item = Base._delete!(h.ht, index)
        delete!(h.order, item.idx)
        fixidx(h.order, item.idx, endof(h.order))
        item.value
    else
        default
    end
end

start(t::SimpleOrderedDict) = start(t.order)
done(t::SimpleOrderedDict, i) = done(t.order, i)
next(t::SimpleOrderedDict, i) = ((item, n) = next(t.order, i); ((item.key, item.value), n))

end # module
