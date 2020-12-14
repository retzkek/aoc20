using Test
using Chain

testinput = """
    939
    7,13,x,x,59,x,31,19
    """

function readinput(io)
    arrival = parse(Int,readline(io))
    buses = @chain readline(io) begin
        split(_, ",")
        map(x->x=="x" ? -1 : parse(Int,x),_)
    end
    return arrival,buses
end
@test readinput(IOBuffer(testinput)) == (939,[7,13,-1,-1,59,-1,31,19])

nextarrivals(after::Int, buses::Vector{Int})::Vector{Int} =
    map(x->ceil(after/x)*x-after, filter(x->x>0, buses))
@test nextarrivals(readinput(IOBuffer(testinput))...) == [6,10,5,22,11]

function part1()
    open("input/day13.txt", "r") do io
        arrival, buses = readinput(io)
        buses = filter(x->x>0, buses)
        minwait = arrival
        bus = 0
        for (b,n) in zip(buses,nextarrivals(arrival, buses))
            if n < minwait
                bus = b
                minwait = n
            end
        end
        println(bus*minwait)
    end
end
part1()

# from https://rosettacode.org/wiki/Chinese_remainder_theorem#Julia
function chineseremainder(n::Array, a::Array)
    Π = prod(n)
    mod(sum(ai * invmod(Π ÷ ni, ni) * (Π ÷ ni) for (ni, ai) in zip(n, a)), Π)
end

function firstsequential(buses::Vector{Int})::Int
    ns = filter(x->x>0, buses)
    as = [1-i for i in filter(x->buses[x]>0, 1:length(buses))]
    @debug "firstsequential" ns as
    return chineseremainder(ns, as)
end
@testset "firstsequential" begin
    @test firstsequential([17,-1,13,19]) == 3417
    @test firstsequential([ 67,7,59,61]) == 754018
    @test firstsequential([67,-1,7,59,61]) == 779210
    @test firstsequential([67,7,-1,59,61]) == 1261476
    @test firstsequential([1789,37,47,1889]) == 1202161486
end

function part2()
    open("input/day13.txt", "r") do io
        _, buses = readinput(io)
        println(firstsequential(buses))
    end
end
part2()
