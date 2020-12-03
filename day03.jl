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
#println(readmap(IOBuffer(testmap)))

# ok, this comprehension is getting a bit.. uncomprehensible :/
trees(map, slope) =  sum([map[j][1+(slope*(j-1)%length(map[j]))] for j in 1:size(map,1)] .== '#')
@test trees(readmap(IOBuffer(testmap)),3) == 7

function main()
    m = readmap("input/day03.txt")
    println(trees(m,3))
end

main()
