using Test

const testinput = b"""
    16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4"""

const testinput2 = b"""
    28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3"""

function readnums(type, in)
    eachline(in) .|> x->parse(type,x)
end

function differences(v::Vector{T})::Tuple{Int,Int} where (T<:Number)
    # Treat the charging outlet near your seat as having an effective joltage
    # rating of 0.
    a = [0;sort(v)]
    # your device has a built-in joltage adapter rated for 3 jolts higher than
    # the highest-rated adapter in your bag
    b = [a[2:end];a[end]+3]
    d = b-a
    return sum(d.==1), sum(d.==3)
end
@test readnums(Int, IOBuffer(testinput)) |> differences == (7,5)
@test readnums(Int, IOBuffer(testinput2)) |> differences == (22,10)

function arrangements(v::Vector{T})::Int where (T<:Number)
    sv=[0;sort(v)]
    origins = Dict{Int,Int}(0=>1)
    for n in sv[2:end]
        origins[n] = sum([origins[x] for x in filter(y->n-3<=y<n, sv)])
        @debug "arrangements" n origins[n]
    end
    return origins[maximum(sv)]
end
@test readnums(Int, IOBuffer(testinput)) |> arrangements == 8
@test readnums(Int, IOBuffer(testinput2)) |> arrangements == 19208


function main()
    readnums(Int, "input/day10.txt") |> differences |> x->x[1]*x[2] |> println
    readnums(Int, "input/day10.txt") |> arrangements |> println
end


main()
