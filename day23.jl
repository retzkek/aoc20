using Test

const testinput = "389125467"

function crabcups(ins::AbstractString, moves::Int)::String
    ns = [parse(Int,x) for x in ins]
    @debug "input" ns
    for i in 1:moves
        ds = filter(x->x<ns[1], @view ns[5:end])
        d = isempty(ds) ? maximum(@view ns[5:end]) : maximum(ds)
        id = findfirst(x->x==d, ns)
        ns = [ns[5:id]; ns[2:4]; ns[id+1:end]; ns[1]]
        @debug "round $i" ns
    end
    i1 = findfirst(x->x==1, ns)
    join([ns[i1+1:end];ns[1:i1-1]])
end
@test crabcups(testinput, 10) == "92658374"
@test crabcups(testinput, 100) == "67384529"

const input = "925176834"

function part1()
    println(crabcups(input, 100))
end
