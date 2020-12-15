using Test


readinput(io) =  map(x->parse(Int,x), split(readline(io), ","))
@test readinput(IOBuffer(b"0,3,6")) == [0,3,6]

function next(ns)
    p=findprev(x->x==ns[end], ns, length(ns)-1)
    isnothing(p) ? 0 : length(ns)-p
end
@testset "next" begin
    @test next([0,3,6]) == 0
    @test next([0,3,6,0]) == 3
    @test next([0,3,6,0,3]) == 3
    @test next([0,3,6,0,3,3]) == 1
    @test next([0,3,6,0,3,3,1]) == 0
    @test next([0,3,6,0,3,3,1,0]) == 4
    @test next([0,3,6,0,3,3,1,0,4]) == 0
end

function nth(seed, n)
    ns = Vector{Int}(undef, n)
    for i in 1:length(seed)
        ns[i] = seed[i]
    end
    for i in length(seed)+1:n
        ns[i] = next(@view ns[1:i-1])
    end
    ns[end]
end
@testset "nth" begin
    @test nth([0,3,6], 2020) == 436
    @test nth([1,3,2], 2020) == 1
    @test nth([2,1,3], 2020) == 10
    @test nth([1,2,3], 2020) == 27
    @test nth([2,3,1], 2020) == 78
    @test nth([3,2,1], 2020) == 438
    @test nth([3,1,2], 2020) == 1836
end

const input = b"17,1,3,16,19,0"

function part1()
    println(nth(readinput(IOBuffer(input)),2020))
end
part1()

function part2()
    println(nth(readinput(IOBuffer(input)),30000000))
end
part2()
