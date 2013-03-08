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
#   push!(od, (k,v))     # Adds (k,v) to the end of the dictionary
#   pop!(od)             # Removes and returns the last key-value pair
#   unshift!(od, (k,v))  # Adds (k,v) to the front of the dictionary
#   shift!(od)           # Removes and returns the first key-value pair
#   append!(od, items)   # Adds (k,v) pairs from items to the end of
#                        # the dictionary
#
# Note also that this is not a sorted dictionary, although it can be
# sorted with 
#
#   sort!(od)            # od is an OrderedDict()
#
# You can also sort normal dictionaries, and get a sorted OrderedDict
# back:
#
#   od = sort(d)         # d is a Dict; returns a sorted OrderedDict
#

# Construction
import Base.similar, Base.sizehint

# Serialization
import Base.serialize, Base.deserialize

# Iteration
import Base.start, Base.next, Base.done

# General Collections
import Base.isempty, Base.empty!, Base.length

# Indexable Collections
import Base.assign, Base.ref

# Associative Collections
import Base.has, Base.get, Base.getkey, Base.delete!

# Dequeue-like
import Base.push!, Base.pop!, Base.unshift!, Base.shift!, Base.append! #, Base.insert!  ## TODO

# Useful, unexported by Base
import Base.findnextnot, Base.findfirstnot   ## from bitarray.jl
import Base._tablesz, Base.hashindex         ## from dict.jl

# Sorting

import Base.sort!, Base.sort, Base.sortby!, Base.sortby, Base.sortperm
import Sort.DEFAULT_UNSTABLE, Sort.DEFAULT_STABLE,
       Sort.Ordering, Sort.Algorithm, 
       Sort.Forward, Sort.Reverse,
       Sort.By, Sort.Lt, Sort.lt

######################
## OrderedDict type ##

type OrderedDict{K,V} <: Associative{K,V}
    slots::Array{Uint8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ord_idxs::Array{Int,1}
    ord_slots::BitArray
    ord::Array{Int,1}
    ndel::Int
    odel::Int
    count::Int
    deleter::Function

    function OrderedDict()
        n = 16
        new(zeros(Uint8,n), Array(K,n), Array(V,n), Array(Int,n), BitArray(), Array(Int,0), 0, 0, 0, identity)
    end
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

##########################
## Construction-related ##

similar{K,V}(d::OrderedDict{K,V}) = OrderedDict{K,V}()

function sizehint(d::OrderedDict, newsz)
    oldsz = length(d.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    rehash(d, newsz)
end

## TODO: some of these are simply copied from base/dict.jl,
##       and the type was changed from Dict -> OrderedDict
##
##       (Field names might also be slightly different, but
##       can be reverted.)
##
##       It would be nice if they were defined in terms
##       of an AbstractDict <: Associative

###################
## Serialization ##
# TODO: Remove these if AbstractDict versions become available

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

########################################
## Dict/OrderedDict Utility functions ##

# TODO: Remove these if AbstractDict versions become available
isslotempty(h::OrderedDict, i::Int) = h.slots[i] == 0x0
isslotfilled(h::OrderedDict, i::Int) = h.slots[i] == 0x1
isslotmissing(h::OrderedDict, i::Int) = h.slots[i] == 0x2

# OrderedDict version of rehash
function rehash{K,V}(h::OrderedDict{K,V}, newsz)
    newsz = _tablesz(newsz)
    nel = h.count
    h.ndel = h.count = 0
    if nel == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        resize!(h.ord_idxs, newsz)
        resize!(h.ord_slots, 0)
        resize!(h.ord, 0)
        return h
    end
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    oldi = h.ord_idxs
    sz = length(olds)
    h.slots = zeros(Uint8,newsz)
    h.keys = Array(K, newsz)
    h.vals = Array(V, newsz)
    h.ord_idxs = Array(Int, newsz)

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            index = hashindex(k, newsz)
            while h.slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            h.slots[index] = 0x1
            h.keys[index] = k
            h.vals[index] = oldv[i]

            idx = oldi[i]
            h.ord_idxs[index] = idx
            h.ord[idx] = index
            h.count += 1
        end
    end

    return h
end

# get the index where a key is stored, or -1 if not present
# TODO: remove if AbstractDict version becomes available
function ht_keyindex{K,V}(h::OrderedDict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
    keys = h.keys

    while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h,index) && isequal(key,keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    return -1
end


# Removes empty slots of order array in OrderedDict
function _compact(h::OrderedDict)
    # start with first empty slot
    npos = findfirstnot(h.ord_slots)
    if npos == 0 return; end
    opos = findnext(h.ord_slots, npos)
    
    # fill down empty slots with consecutive filled slots
    while opos != 0
        index = h.ord[npos] = h.ord[opos]
        h.ord_idxs[index] = npos
        
        npos += 1
        opos = findnext(h.ord_slots, opos+1)
    end
    
    resize!(h.ord, h.count)
    resize!(h.ord_slots, h.count)
    h.ord_slots[1:end] = true
    h.odel = 0

    nothing
end

###############
## Iteration ##

skip_deleted(t::OrderedDict, i::Int) = findnext(t.ord_slots, i)

start(t::OrderedDict) = skip_deleted(t,1)
done(t::OrderedDict, i) = (i == 0)
next(t::OrderedDict, i) = (idx = t.ord[i]; (t.keys[idx],t.vals[idx]), skip_deleted(t,i+1))

#########################
## General Collections ##

isempty(t::OrderedDict) = (t.count == 0)
length(t::OrderedDict) = t.count

function empty!{K,V}(h::OrderedDict{K,V})
    fill!(h.slots, 0x0)
    sz = length(h.slots)
    h.keys = Array(K, sz)
    h.vals = Array(V, sz)
    h.ord_idxs = Array(Int, sz)
    h.ord_slots = BitArray()
    h.ord = Array(Int, 0)
    h.ndel = 0
    h.odel = 0
    h.count = 0
    return h
end

###########################
## Indexable Collections ##

function assign{K,V}(h::OrderedDict{K,V}, v, key)
    key = convert(K,key)
    v   = convert(V,  v)

    sz = length(h.keys)

    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        sz = length(h.keys)  # rehash may resize the table at this point!
    end

    if h.odel >= ((3*length(h.ord))>>2)
        # > 3/4 of ord array deleted
        _compact(h)
    end

    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
    avail = -1  # an available slot
    #keys = h.keys; vals = h.vals; ord_idxs = h.ord_idxs; ord_slots = h.ord_slots; ord = h.ord

    while true
        if isslotempty(h,index)
            if avail > 0; index = avail; end
            push!(h.ord, index)
            push!(h.ord_slots, true)
            h.slots[index] = 0x1
            h.keys[index] = key
            h.vals[index] = v
            h.ord_idxs[index] = length(h.ord)
            h.count += 1
            return h
        end

        if isslotmissing(h,index)
            if avail<0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = index
            end
        elseif isequal(key, h.keys[index])
            h.vals[index] = v
            return h
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    if avail>0
        index = avail
        push!(h.ord, index)
        push!(h.ord_slots, true)
        h.slots[index] = 0x1
        h.keys[index] = key
        h.vals[index] = v
        h.ord_idxs[index] = length(h.ord)
        h.count += 1
        return h
    end

    rehash(h, h.count > 64000 ? sz*2 : sz*4)

    assign(h, v, key)
end

# TODO: remove if AbstractDict version becomes available
function ref{K,V}(h::OrderedDict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

#############################
## Associative Collections ##

has(h::OrderedDict, key) = (ht_keyindex(h, key) >= 0)

function get{K,V}(h::OrderedDict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]::V
end

function getkey{K,V}(h::OrderedDict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]::K
end

function _delete!(h::OrderedDict, index)
    val = h.vals[index]
    idx = h.ord_idxs[index]
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    # h.ord_idxs[index] = 0  ## not really necessary
    # ord[idx] = 0           ## not really necessary
    h.ord_slots[idx] = false
    h.ndel += 1
    h.odel += 1
    h.count -= 1
    return val
end

function delete!(h::OrderedDict, key)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : throw(KeyError(key))
end

function delete!(h::OrderedDict, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : default
end

##################
## Dequeue-like ##

# Add key-value pair at last slot
# TODO: should this be the only push, so we can handle multiple items?
push!(t::OrderedDict, item::(Any,Any)) = push!(t, item[1], item[2])
push!(t::OrderedDict, k, v) = (delete!(t, k, nothing); t[k] = v; (k,v))

# Remove and return last key-value pair
function pop!(t::OrderedDict)
    if isempty(t)
        error("pop!: OrderedDict is empty")
    end
    #idx = findlast(t.ord_slots) ## TODO: create this!
    ######
    local idx
    for idx = length(t.ord):-1:1
        if t.ord_slots[idx] break end
    end
    ######
    k = t.keys[t.ord[idx]]
    (k, delete!(t, k))
end

# Put key-value pair at front of dict
function unshift!(t::OrderedDict, k, v)
    # Note: O(N)!
    # Add item, then move to beginning
    delete!(t, k, nothing)
    t[k] = v
    if t.odel == 0
        unshift!(t.ord, pop!(t.ord))
        for i = 1:length(t.ord)
            t.ord_idxs[t.ord[i]] = i
        end
    else
        first_empty_idx = findfirstnot(t.ord_slots)
        t.ord_slots[first_empty_idx] = true
        new_ht_idx = pop!(t.ord)
        pop!(t.ord_slots)
        for i = first_empty_idx:-1:2
            ht_idx = t.ord[i] = t.ord[i-1]
            ord_idxs[ht_idx] = i
        end
        t.ord[1] = new_ht_idx
        t.ord_idxs[new_ht_idx] = 1
    end
    (k,v)
end
# TODO: should this be the only unshift, so we can handle multiple items?
unshift!(t::OrderedDict, item::(Any,Any)) = unshift!(t, item[1], item[2])

# Remove and return first key-value pair
function shift!(t::OrderedDict)
    if isempty(t)
        error("shift!: OrderedDict is empty")
    end
    idx = findfirst(t.ord_slots)
    k = t.keys[t.ord[idx]]
    (k, delete!(t,k))
end

# Add multiple items to dictionary, at end
function append!(t::OrderedDict, items)
    for (k,v) in items
        t[k] = v
    end
    items
end

# Sorting
function sort!(t::OrderedDict, args...)
    _compact(t)
    p = sortperm(t.keys[t.ord], args...)
    t.ord[:] = t.ord[p]
    for i = 1:length(t.ord)
        t.ord_idxs[t.ord[i]] = i
    end
    t
end

function sort{K,V}(t::Dict{K,V}, args...)
    d = OrderedDict{K,V}()
    sizehint(d, length(t.slots))
    for k in sort(keys(t))
        d[k] = t[k]
    end
    d
end

sortperm(v::OrderedDict, args...) = sortperm(v.keys[v.ord], args...)

# TODO: with multiple inheritance (or simplification of the sort module)
# almost everything below could be removed
# (This was simply copy and pasted from base/sort.jl, and AbstractVector was changed to OrderedDict)

sort (v::OrderedDict, a::Algorithm, o::Ordering) = sort!(copy(v), a, o)

sort!(v::OrderedDict, o::Ordering) = sort!(v, DEFAULT_STABLE, o)
sort (v::OrderedDict, o::Ordering) = sort (v, DEFAULT_STABLE, o)

for s in {:sort!, :sort, :sortperm}
    @eval begin
        # default to forward sort ordering
        $s(v::OrderedDict, a::Algorithm) = $s(v, a, Forward())
        $s(v::OrderedDict              ) = $s(v,    Forward())

        # auto-instntiate algorithms and orderings from types
        $s{A<:Algorithm,O<:Ordering}(v::OrderedDict, ::Type{A},    ::Type{O})   = $s(v, A(), O())
        $s{A<:Algorithm            }(v::OrderedDict, ::Type{A},    o::Ordering) = $s(v, A(), o)
        $s{             O<:Ordering}(v::OrderedDict, a::Algorithm, ::Type{O})   = $s(v, a,   O())
        $s{A<:Algorithm            }(v::OrderedDict, ::Type{A})                 = $s(v, A())
        $s{             O<:Ordering}(v::OrderedDict,               ::Type{O})   = $s(v,      O())

        # also allow ordering before algorithm
        $s                          (v::OrderedDict, o::Ordering, a::Algorithm) = $s(v, a, o)
        $s{A<:Algorithm,O<:Ordering}(v::OrderedDict, ::Type{O},   ::Type{A})    = $s(v, A(), O())
        $s{A<:Algorithm            }(v::OrderedDict, o::Ordering, ::Type{A})    = $s(v, A(), o)
        $s{             O<:Ordering}(v::OrderedDict, ::Type{O},   a::Algorithm) = $s(v, a,   O())
    end
end

for s in {:sort!, :sort, :sortperm}
    @eval begin
        $s{A<:Algorithm}(v::OrderedDict, a::Union(A,Type{A}), lt::Function) = $s(v, a, Sort.Lt(lt))
        $s{A<:Algorithm}(v::OrderedDict, lt::Function, a::Union(A,Type{A})) = $s(v, a, lt)
        $s              (v::OrderedDict, lt::Function)                      = $s(v, Sort.Lt(lt))
        $s              (lt::Function, v::OrderedDict, args...)             = $s(v, lt, args...)
    end
end

for (sb,s) in {(:sortby!, :sort!), (:sortby, :sort), (:sortpermby, :sortperm)}
    @eval begin
        $sb{A<:Algorithm}(v::OrderedDict, a::Union(A,Type{A}), by::Function) = $s(v, a, Sort.By(by))
        $sb{A<:Algorithm}(v::OrderedDict, by::Function, a::Union(A,Type{A})) = $s(v, a, Sort.By(by))
        $sb              (v::OrderedDict, by::Function)                      = $s(v, Sort.By(by))
        $sb              (by::Function, v::OrderedDict, args...)             = $s(v, Sort.By(by), args...)
    end
end
