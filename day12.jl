using Test

testinput = b"""
    F10
    N3
    F7
    R90
    F11"""

struct Ship
    x::Int
    y::Int
    dir::Int
end

Ship(x,y) = Ship(x,y,90)
Ship() = Ship(0,0)


function translate(b::Ship, s::String)::Ship
    m = match(r"([NSEWLRF])(\d+)", s)
    if isnothing(m) return b end
    dir = m.captures[1]
    dist = parse(Int, m.captures[2])
    if dir == "N" || (dir == "F" && b.dir == 0)
        Ship(b.x, b.y-dist, b.dir)
    elseif dir == "E" || (dir == "F" && b.dir == 90)
        Ship(b.x+dist, b.y, b.dir)
    elseif dir == "S" || (dir == "F" && b.dir == 180)
        Ship(b.x, b.y+dist, b.dir)
    elseif dir == "W" || (dir == "F" && b.dir == 270)
        Ship(b.x-dist, b.y, b.dir)
    elseif dir == "L"
        Ship(b.x, b.y, (b.dir+360-(dist%360))%360)
    elseif dir == "R"
        Ship(b.x, b.y, (b.dir+dist)%360)
    end
end
@testset "translate" begin
    @test translate(Ship(),"N10") == Ship(0,-10,90)
    @test translate(Ship(),"E10") == Ship(10,0,90)
    @test translate(Ship(),"S10") == Ship(0,10,90)
    @test translate(Ship(),"W10") == Ship(-10,0,90)
    @test translate(Ship(),"L90") == Ship(0,0,0)
    @test translate(Ship(0,0,0),"L90") == Ship(0,0,270)
    @test translate(Ship(),"R90") == Ship(0,0,180)
    @test translate(Ship(0,0,270),"R90") == Ship(0,0,0)
    @test translate(Ship(0,0,0),"F10") == Ship(0,-10,0)
    @test translate(Ship(),"F10") == Ship(10,0,90)
    @test translate(Ship(0,0,180),"F10") == Ship(0,10,180)
    @test translate(Ship(0,0,270),"F10") == Ship(-10,0,270)
end

manhattan(b::Ship)::Int = abs(b.x)+abs(b.y)
move(b::Ship, in)::Ship = reduce(translate, eachline(in); init=b)

@test move(Ship(), IOBuffer(testinput)) |> manhattan == 25

function part1()
    move(Ship(), "input/day12.txt") |> manhattan |> println
end

part1()
