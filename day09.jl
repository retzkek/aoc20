using Test

mutable struct Ring{T}
    length::Int
    v::Array{T,1}
    i::Int
end

function Ring(T::DataType,length::Int)
    Ring{T}(length, Array{T,1}(undef, length), 1)
end

function Ring(v::AbstractArray)
    Ring{eltype(v)}(length(v), v, 1)
end

# defining this is Base doesn't seem quite right, but
# if I don't I get an error for other methods of ==, e.g.:
#   MethodError: no method matching ==(::Int64, ::Int64)
#   You may have intended to import Base.:(==)
function Base.:(==)(a::Ring, b::Ring)
    a.length == b.length && a.v == b.v && a.i==b.i
end

@testset "Ring constructors" begin
    r=Ring(Int,25) # can't directly test equality since the internal array values are undef
    @test r.length == 25
    @test eltype(r.v) == Int
    @test length(r.v) == 25
    @test r.i == 1

    @test Ring([1,2,3]) == Ring(3, [1,2,3], 1)

    @test Ring(1:25) == Ring(25, convert(Array{Int,1},1:25), 1)
end

function insert!(r::Ring{T}, val::T) where (T)
    r.v[r.i] = val
    r.i = r.i%r.length+1
end

@testset "Ring insert!" begin
    r = Ring(1:3)
    @test r.v == [1,2,3]
    insert!(r, 4)
    @test r.v == [4,2,3]
    insert!(r, 5)
    @test r.v == [4,5,3]
    insert!(r, 6)
    @test r.v == [4,5,6]
    insert!(r, 7)
    @test r.v == [7,5,6]
end

function sums(r::Ring)
    Set([x+y for x in r.v for y in filter(n->n!=x,r.v)])
end

@testset "Ring sums" begin
    r = Ring(1:3)
    @test sums(r) == Set([3, 4, 5])
    r = Ring([20;1:19;21:25])
    s = sums(r)
    @test 26 ∈ s
    @test 49 ∈ s
    @test 100 ∉ s
    @test 50 ∉ s
    insert!(r,45)
    s = sums(r)
    @test 26 ∈ s
    @test 65 ∉ s
    @test 64 ∈ s
    @test 66 ∈ s
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

function xmas(in, len)
    r = Ring(Int, len)
    i = 0
    for line in eachline(in)
        i += 1
        n = parse(Int,line)
        if i <= len || n ∈ sums(r)
            insert!(r,n)
        else
            return n
        end
    end
    return -1
end
@test xmas(IOBuffer(testdata),5) == 127

function main()
    xmas("input/day09.txt", 25) |> println
end

main()
