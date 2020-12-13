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
        filter(x->x!="x",_)
        map(x->parse(Int,x),_)
    end
    return arrival,buses
end
@test readinput(IOBuffer(testinput)) == (939,[7,13,59,31,19])

nextarrivals(after::Int, buses::Vector{Int})::Vector{Int} =
    map(x->ceil(after/x)*x-after, buses)
@test nextarrivals(readinput(IOBuffer(testinput))...) == [6,10,5,22,11]

function part1()
    open("input/day13.txt", "r") do io
        arrival, buses = readinput(io)
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
