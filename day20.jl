using Test

const testinput = b"""
    Tile 2311:
    ..##.#..#.
    ##..#.....
    #...##..#.
    ####.#...#
    ##.##.###.
    ##...#.###
    .#.#.#..##
    ..#....#..
    ###...#.#.
    ..###..###

    Tile 1951:
    #.##...##.
    #.####...#
    .....#..##
    #...######
    .##.#....#
    .###.#####
    ###.##.##.
    .###....#.
    ..#.#..#.#
    #...##.#..

    Tile 1171:
    ####...##.
    #..##.#..#
    ##.#..#.#.
    .###.####.
    ..###.####
    .##....##.
    .#...####.
    #.##.####.
    ####..#...
    .....##...

    Tile 1427:
    ###.##.#..
    .#..#.##..
    .#.##.#..#
    #.#.#.##.#
    ....#...##
    ...##..##.
    ...#.#####
    .#.####.#.
    ..#..###.#
    ..##.#..#.

    Tile 1489:
    ##.#.#....
    ..##...#..
    .##..##...
    ..#...#...
    #####...#.
    #..#.#.#.#
    ...#.#.#..
    ##.#...##.
    ..##.##.##
    ###.##.#..

    Tile 2473:
    #....####.
    #..#.##...
    #.##..#...
    ######.#.#
    .#...#.#.#
    .#########
    .###.#..#.
    ########.#
    ##...##.#.
    ..###.#.#.

    Tile 2971:
    ..#.#....#
    #...###...
    #.#.###...
    ##.##..#..
    .#####..##
    .#..####.#
    #..#.#..#.
    ..####.###
    ..#.#.###.
    ...#.#.#.#

    Tile 2729:
    ...#.#.#.#
    ####.#....
    ..#.#.....
    ....#..#.#
    .##..##.#.
    .#.####...
    ####.#.#..
    ##.####...
    ##..#.##..
    #.##...##.

    Tile 3079:
    #.#.#####.
    .#..######
    ..#.......
    ######....
    ####.#..#.
    .#...#.##.
    #.#####.##
    ..#.###...
    ..#.......
    ..#.###...
    """

mutable struct Tile
    id::Int
    edges::Vector{Int}
    redges::Vector{Int}
end
Tile() = Tile(0, zeros(4), zeros(4))

edgenum(e::AbstractString)::Int = parse(Int, map(x->x=='.' ? '0' :
                                                x=='#' ? '1' :
                                                error("unexpected character '$x'"),
                                                e), base=2)

function readtiles(io, size)::Vector{Tile}
    tiles = Vector{Tile}()
    t = Tile()
    left = Vector{Char}(undef,size)
    right = Vector{Char}(undef,size)
    i = 0
    for line in eachline(io)
        i += 1
        if line == ""
            continue
        elseif startswith(line, "Tile")
            m = match(r"Tile (\d+):", line)
            t.id = parse(Int,m[1])
            i = 0
        elseif i == 1
            t.edges[1] = edgenum(line)
            t.redges[1] = edgenum(reverse(line))
            left[i] = first(line)
            right[i] = last(line)
        elseif i == size
            right[i] = last(line)
            t.edges[2] = edgenum(join(right))
            t.redges[2] = edgenum(reverse(join(right)))
            t.edges[3] = edgenum(line)
            t.redges[3] = edgenum(reverse(line))
            left[i] = first(line)
            t.edges[4] = edgenum(join(left))
            t.redges[4] = edgenum(reverse(join(left)))
            push!(tiles,t)
            t = Tile()
        else
            left[i] = first(line)
            right[i] = last(line)
        end
    end
    return tiles
end

function findcorners(tiles::Vector{Tile})::Vector{Int}
    corners = Vector{Int}()
    edges = reduce(append!, [[x.edges;x.redges] for x in tiles])
    for t in tiles
        matches = [count(x->x==e, edges) for e in t.edges]
        if count(x->x==1, matches) == 2
            push!(corners,t.id)
        end
    end
    @assert length(corners) == 4
    return corners
end
@testset "corners" begin
    t=readtiles(IOBuffer(testinput),10)
    @test prod(findcorners(t)) == 20899048083289
end

function part1()
   readtiles("input/day20.txt",10) |> findcorners |> prod |> println
end
