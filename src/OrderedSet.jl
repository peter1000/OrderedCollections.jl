###################
# OrderedSet
#
# This is basic ordered set based on OrderedDicts
#

type OrderedSet{T}
    hash::OrderedDict{T,Bool}
    
    OrderedSet() = new(OrderedDict{T,Bool}())
    OrderedSet(x...) = append!(new(OrderedDict{T,Bool}(), x))
end
