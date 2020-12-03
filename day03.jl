using Test

testmap = b"""
    ..##.......
    #...#...#..
    .#....#..#.
    ..#.#...#.#
    .#...##..#.
    ..#.##.....
    .#.#.#....#
    .#........#
    #.##...#...
    #...##....#
    .#..#...#.#"""

readmap(in) = collect(eachline(in))
@test readmap(IOBuffer(testmap)) == ["..##.......",
                                     "#...#...#..",
                                     ".#....#..#.",
                                     "..#.#...#.#",
                                     ".#...##..#.",
                                     "..#.##.....",
                                     ".#.#.#....#",
                                     ".#........#",
                                     "#.##...#...",
                                     "#...##....#",
                                     ".#..#...#.#"]

function trees(map, xstr, ystr)
    i,j = 0,1
    cnt = 0
    while j <= size(map,1)
        cnt += map[j][1+i] == '#'
        i = (i + xstr)%length(map[j])
        j += ystr
    end
    cnt
end
@testset "counting trees" begin
    m = readmap(IOBuffer(testmap))
    @test trees(m,3,1) == 7
    @test trees(m,1,1) == 2
    @test trees(m,5,1) == 3
    @test trees(m,7,1) == 4
    @test trees(m,1,2) == 2
end

function main()
    m = readmap("input/day03.txt")
    println(trees(m,3,1))
    println(reduce(*,[trees(m,x...) for x in [(1,1),(3,1),(5,1),(7,1),(1,2)]]))
end

main()
