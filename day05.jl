using Chain
using Test

function seat(s)
    @chain s begin
           replace(_,"F"=>"0")
           replace(_,"B"=>"1")
           replace(_,"L"=>"0")
           replace(_,"R"=>"1")
           (parse(Int, _[1:7], base=2),parse(Int, _[8:10], base=2))
    end
end
seatnumber((r,c)) = r*8+c
@testset "seat" begin
    @test seat("FBFBBFFRLR") == (44,5)
    @test seat("BFFFBBFRRR") == (70,7)
    @test seat("FFFBBBFRRR") == (14,7)
    @test seat("BBFFBBFRLL") == (102,4)
    @test seatnumber((44,5)) == 357
end

function main()
    map = falses(128*8)
    last = 0
    for l in eachline("input/day05.txt")
        n = l |> seat |> seatnumber
        last = max(last,n)
        map[n+1] = true
    end
    println(last)
    for i in 0:(last-3)
        if map[i+1:i+3] == [1,0,1]
            println(i+1)
            break
        end
    end
end

main()
