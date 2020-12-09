using Test

function sums(v::AbstractVector)
    Set([x+y for x in v for y in filter(n->n!=x,v)])
end

@testset "sums" begin
    r = 1:3
    @test sums(r) == Set([3, 4, 5])
    r = [20;1:19;21:25]
    s = sums(r)
    @test 26 ∈ s
    @test 49 ∈ s
    @test 100 ∉ s
    @test 50 ∉ s
end

const testdata = b"""
    35
    20
    15
    25
    47
    40
    62
    55
    65
    95
    102
    117
    150
    182
    127
    219
    299
    277
    309
    576"""

function read_xmas(in, len)
    v = Int[]
    i = 0
    for line in eachline(in)
        n = parse(Int,line)
        if length(v) <= len || n ∈ sums(v[end-len+1:end])
            append!(v,n)
        else
            return v,n
        end
    end
    return v,-1
end
@test read_xmas(IOBuffer(testdata),5)[2] == 127

function subvec_sum_eq(v::Vector{T}, s::T) where (T)
    for i in 1:length(v)
        for j in i+1:length(v)
            if sum(v[i:j]) == s
                return  v[i:j]
            end
        end
    end
    return T[]
end
@test subvec_sum_eq([1,2,3,4,5],9) == [2,3,4]

function main()
    v, invalid = read_xmas("input/day09.txt", 25)
    println(invalid)
    sv = subvec_sum_eq(v,invalid)
    @debug "subvec" sv
    println(minimum(sv)+maximum(sv))
end

main()
