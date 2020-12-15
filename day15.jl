using Test


readinput(io) =  map(x->parse(Int,x), split(readline(io), ","))
@test readinput(IOBuffer(b"0,3,6")) == [0,3,6]

function next(ns, lasti, lastn)
    haskey(ns, lastn) ? lasti-ns[lastn] : 0
end
@testset "next" begin
    @test next(Dict(0=>1,3=>2,6=>3), 4, 0) == 3
    @test next(Dict(0=>4,3=>2,6=>3), 5, 3) == 3
end

function nth(seed, n)
    ns = Dict{Int,Int}()
    for i in 1:length(seed)
        ns[seed[i]] = i
    end
    lastn = seed[end]
    nextn = 0
    for i in length(seed)+1:n
        lastn = nextn
        nextn = next(ns, i, lastn)
        ns[lastn] = i
        @debug "ns" i lastn nextn ns
    end
    lastn
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
