using Test
using Match

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
    wx::Int
    wy::Int
end

Ship(x,y,dir) = Ship(x,y,dir,10,1)
Ship(x,y) = Ship(x,y,90)
Ship() = Ship(0,0)


function translate(b::Ship, s::String)::Ship
    m = match(r"([NSEWLRF])(\d+)", s)
    if isnothing(m) return b end
    dir = m.captures[1]
    dist = parse(Int, m.captures[2])
    if dir == "N" || (dir == "F" && b.dir == 0)
        Ship(b.x, b.y+dist, b.dir)
    elseif dir == "E" || (dir == "F" && b.dir == 90)
        Ship(b.x+dist, b.y, b.dir)
    elseif dir == "S" || (dir == "F" && b.dir == 180)
        Ship(b.x, b.y-dist, b.dir)
    elseif dir == "W" || (dir == "F" && b.dir == 270)
        Ship(b.x-dist, b.y, b.dir)
    elseif dir == "L"
        Ship(b.x, b.y, (b.dir+360-(dist%360))%360)
    elseif dir == "R"
        Ship(b.x, b.y, (b.dir+dist)%360)
    end
end
@testset "translate" begin
    @test translate(Ship(),"N10") == Ship(0,10,90)
    @test translate(Ship(),"E10") == Ship(10,0,90)
    @test translate(Ship(),"S10") == Ship(0,-10,90)
    @test translate(Ship(),"W10") == Ship(-10,0,90)
    @test translate(Ship(),"L90") == Ship(0,0,0)
    @test translate(Ship(0,0,0),"L90") == Ship(0,0,270)
    @test translate(Ship(),"R90") == Ship(0,0,180)
    @test translate(Ship(0,0,270),"R90") == Ship(0,0,0)
    @test translate(Ship(0,0,0),"F10") == Ship(0,10,0)
    @test translate(Ship(),"F10") == Ship(10,0,90)
    @test translate(Ship(0,0,180),"F10") == Ship(0,-10,180)
    @test translate(Ship(0,0,270),"F10") == Ship(-10,0,270)
end

manhattan(b::Ship)::Int = abs(b.x)+abs(b.y)
move(b::Ship, in, f)::Ship = reduce(f, eachline(in); init=b)

@test move(Ship(), IOBuffer(testinput), translate) |> manhattan == 25

function part1()
    move(Ship(), "input/day12.txt", translate) |> manhattan |> println
end
part1()

rotateleft(b::Ship,deg)::Ship = deg <= 0 ? b :
    rotateleft(Ship(b.x, b.y, b.dir, -b.wy, b.wx), deg-90)
rotateright(b::Ship,deg)::Ship = deg <= 0 ? b :
    rotateright(Ship(b.x, b.y, b.dir, b.wy, -b.wx), deg-90)

function waypoint(b::Ship, s::String)::Ship
    m = match(r"([NSEWLRF])(\d+)", s)
    if isnothing(m) return b end
    dir = m.captures[1]
    dist = parse(Int, m.captures[2])
    @match dir begin
        "N" => Ship(b.x, b.y, b.dir, b.wx, b.wy+dist)
        "E" => Ship(b.x, b.y, b.dir, b.wx+dist, b.wy)
        "S" => Ship(b.x, b.y, b.dir, b.wx, b.wy-dist)
        "W" => Ship(b.x, b.y, b.dir, b.wx-dist, b.wy)
        "F" => Ship(b.x+dist*b.wx, b.y+dist*b.wy, b.dir, b.wx, b.wy)
        "L" => rotateleft(b, dist)
        "R" => rotateright(b, dist)
    end
end
@testset "waypoint" begin
    @test waypoint(Ship(),"N10") == Ship(0,0,90,10,11)
    @test waypoint(Ship(),"E10") == Ship(0,0,90,20,1)
    @test waypoint(Ship(),"S10") == Ship(0,0,90,10,-9)
    @test waypoint(Ship(),"W10") == Ship(0,0,90,0,1)
    @test waypoint(Ship(),"F10") == Ship(100,10,90,10,1)
    @test waypoint(Ship(),"L90") == Ship(0,0,90,-1,10)
    @test waypoint(Ship(),"L180") == Ship(0,0,90,-10,-1)
    @test waypoint(Ship(),"R90") == Ship(0,0,90,1,-10)
    @test waypoint(Ship(),"R180") == Ship(0,0,90,-10,-1)
end

@test move(Ship(), IOBuffer(testinput), waypoint) |> manhattan == 286

function part2()
    move(Ship(), "input/day12.txt", waypoint) |> manhattan |> println
end
part2()
