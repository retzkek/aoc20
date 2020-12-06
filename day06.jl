using Test
using Chain

testdata = b"""
    abc

    a
    b
    c

    ab
    ac

    a
    a
    a
    a

    b
    """

function count_answers(in, comb)
    answers = []
    grp = []
    for line in eachline(in)
        if line == "" && length(grp) > 0
            push!(answers,length(comb(grp...)))
            grp=[]
        else
            push!(grp, split(line,""))
        end
    end
    if length(grp) > 0
        push!(answers,length(comb(grp...)))
    end
    answers |> sum
end
@testset "count_answers" begin
    d=IOBuffer(testdata)
    @test count_answers(d, ∪) == 11
    seek(d,0)
    @test count_answers(d, ∩) == 6
end

function main()
    count_answers("input/day06.txt", ∪) |> println
    count_answers("input/day06.txt", ∩) |> println
end

main()
