using Test

const testinput = b"""
    Player 1:
    9
    2
    6
    3
    1

    Player 2:
    5
    8
    4
    7
    10
    """

Deck = Vector{Int}

function readdecks(in)::Tuple{Deck, Deck}
    d1 = Vector{Int}()
    d2 = Vector{Int}()
    d = d1
    for line in eachline(in)
        if line == ""
            continue
        elseif line == "Player 1:"
            d = d1
        elseif line == "Player 2:"
            d = d2
        else
            push!(d, parse(Int,line))
        end
    end
    d1,d2
end
@test readdecks(IOBuffer(testinput)) == ([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])

deckscore(d::Deck) = sum(d .* (length(d):-1:1))
@test deckscore([3, 2, 10, 6, 8, 5, 9, 4, 7, 1]) == 306

function combat(d1::Deck, d2::Deck)
    i = 1
    while length(d1) > 0 && length(d2) > 0
        c1 = popfirst!(d1)
        c2 = popfirst!(d2)
        if c1 > c2
            push!(d1,c1,c2)
        else
            push!(d2,c2,c1)
        end
        @debug "round $i" c1 c2 d1 d2
        i += 1
    end
    length(d1) > 0 ? deckscore(d1) : deckscore(d2)
end
@test combat(readdecks(IOBuffer(testinput))...) == 306

function part1()
    combat(readdecks("input/day22.txt")...) |> println
end
