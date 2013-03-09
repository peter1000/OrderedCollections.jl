
using Benchmark
require("OrderedDict.jl")
require("OrderedDictAlt.jl")

n = 100000
strs = [randstring(10) for i = 1:n];
nums = rand(Int, n);

# regular map

function dict_insertion_test{T<:Associative}(d::T)
    #empty!(d)
    for i = 1:n
        d[strs[i]] = nums[i]
    end
    d
end

function dict_deletion_test{T<:Associative}(d::T, iters::Int)
    #dict_insertion_test(d)
    for i in rand(1:n, iters)
        delete!(d, strs[i], 0)
    end
    d
end

function dict_ins_del_test{T<:Associative}(d::T, iters::Int)
    #dict_insertion_test(d)
    for i in rand(1:n, iters)
        randbool()? delete!(d, strs[i], 0) : (d[strs[i]] = nums[i])
    end
    d
end    


function test_ins(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      gc()
      t += @elapsed dict_insertion_test(d)
   end
   t
end

function test_ins_del(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      dict_insertion_test(d)
      gc()
      t += @elapsed dict_ins_del_test(d, 100000)
   end
   t
end

function test_del(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      dict_insertion_test(d)
      gc()
      t += @elapsed dict_deletion_test(d, 100000)
   end
   t
end

d = Dict{String,Int}()
od=OrderedDict{String,Int}()
oda=OrderedDictAlt{String,Int}()

       
compare([()->dict_insertion_test(d), ()->dict_insertion_test(od3)], int(1000000/n))
#compare([()->dict_insertion_test(d), ()->dict_insertion_test(od), ()->dict_insertion_test(od3)], int(1000000/n))
#
# n = 1_000_000
#
#            Function Elapsed Relative Replications
#[1,]    "# function" 2.43333      1.0            1
#[2,]    "# function" 3.13963  1.29026            1
#[3,]    "# function" 5.05498   2.0774            1
###########
# n = 100_000
#
#            Function  Elapsed Relative Replications
#[1,]    "# function" 0.593007      1.0          100
#[2,]    "# function" 0.881763  1.48694          100
#[3,]    "# function"  1.24341  2.09679          100


#compare([()->dict_ins_del_test(d,n), ()->dict_ins_del_test(od,n), ()->dict_ins_del_test(od3,n)], int(100000/n))
compare([()->dict_ins_del_test(d,n), ()->dict_ins_del_test(od3,n)], int(100000/n))
#
# n = 1_000_000
#
#            Function Elapsed Relative Replications
#[1,]    "# function" 4.72177      1.0            1
#[2,]    "# function" 57.4363  12.1641            1
#[3,]    "# function" 38.0523  8.05891            1
###########
# n = 100_000
#
#            Function  Elapsed Relative Replications
#[1,]    "# function" 0.078143      1.0           10
#[2,]    "# function"  5.26876  67.4246           10
#[3,]    "# function"  2.62656  33.6122           10


compare([()->dict_deletion_test(d,n), ()->dict_deletion_test(od3,n)], int(1000000/n))
#compare([()->dict_deletion_test(d,n), ()->dict_deletion_test(od,n), ()->dict_deletion_test(od3,n)], int(1000000/n))
#
# n = 1_000_000
#
#[1,]    "# function" 0.16877      1.0            1
#[2,]    "# function" 10.2882  60.9598            1
#[3,]    "# function" 8.07296  47.8342            1
###########
# n = 100_000
#
#            Function   Elapsed Relative Replications
#[1,]    "# function" 0.0640104      1.0           10
#[2,]    "# function"    10.208  159.475           10
#[3,]    "# function"   9.93042  155.138           10


