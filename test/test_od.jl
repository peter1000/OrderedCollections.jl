using OrderedCollections
using Test

# Test OrderedDict creation
od = OrderedDict{String,Int}()
for i = 1:factorial(5)
   od[join(nthperm(split("abcde",""),i))] = i
end
@test length(od) == factorial(5)

ks = [join(nthperm(split("abcde",""),i)) for i = 1:factorial(5)]
vs = 1:factorial(5)
od2 = OrderedDict(ks,vs)
od3 = copy(od2)
od4 = copy(od2)
od5 = copy(od2)

# Test equality
@test od == od2

# Key-value access
@test keys(od) == ks
@test values(od) == vs

@test all([(indexof(od, ks[i]) == i) for i = 1:factorial(5)])

# Index access
@test od4[1] == ("abcde", 1)
@test od4[120] == ("edcba", 120)

@test all([od[i] == (join(nthperm(split("abcde",""),i)),i) for i = 1:factorial(5)])

# Deletion, compaction
for i = 21:factorial(5)
    delete!(od, ks[i])
end

@test all([(indexof(od, ks[i]) == i) for i = 1:20])
@test length(od) == 20

for i = 2:2:factorial(5)
    delete!(od2, ks[i])   # delete by key
    delete!(od3, i)       # delete by numerical index
end

@test all([(indexof(od2, ks[i]) == int(i/2)) for i = 1:2:factorial(5)])
@test length(od2) == 60
@test od2 == od3

@test isempty(od) == false
for i = 1:20
    delete!(od, ks[i])
end
@test isempty(od) == true

@test isempty(od2) == false
empty!(od2)
@test isempty(od2) == true

@test collect(od4) == [(k,v) for (k,v) in zip(ks,vs)]

## TODO: broken
# io = IOString()
# serialize(io, od2)
# seek(io, 0)
# od3 = deserialize(io)

## TODO: implement
# first(od4) == ("abcde", 1)
# last(od4) == od4[end] == ("edcba", 120)
# reverse(od3)

push!(od4, ("hi", 0))
@test od4[length(od4)] == ("hi", 0)
unshift!(od4, ("bye", 100))
@test od4[1] == ("bye", 100)

@test pop!(od4) == ("hi", 0)
@test shift!(od4) == ("bye", 100)
@test od4 == od5
