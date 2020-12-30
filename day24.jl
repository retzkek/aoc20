using Test

const testinput = b"""
    sesenwnenenewseeswwswswwnenewsewsw
    neeenesenwnwwswnenewnwwsewnenwseswesw
    seswneswswsenwwnwse
    nwnwneseeswswnenewneswwnewseswneseene
    swweswneswnenwsewnwneneseenw
    eesenwseswswnenwswnwnwsewwnwsene
    sewnenenenesenwsewnenwwwse
    wenwwweseeeweswwwnwwe
    wsweesenenewnwwnwsenewsenwwsesesenwne
    neeswseenwwswnwswswnw
    nenwswwsewswnenenewsenwsenwnesesenew
    enewnwewneswsewnwswenweswnenwsenwsw
    sweneswneswneneenwnewenewwneswswnese
    swwesenesewenwneswnwwneseswwne
    enesenwswwswneneswsenwnewswseenwsese
    wnwnesenesenenwwnenwsewesewsesesew
    nenewswnwewswnenesenwnesewesw
    eneswnwswnwsenenwnwnwwseeswneewsenese
    neswnwewnwnwseenwseesewsenwsweewe
    wseweeenwnesenwwwswnew
    """

function tile(dir::AbstractString)::Tuple{Int,Int}
    j, i = 0,0
    c = 1
    while c <= length(dir)
        if dir[c] == 'e'
            i += 1
            c += 1
        elseif dir[c] == 'w'
            i -= 1
            c += 1
        elseif dir[c] == 's'
            j += 1
            if dir[c+1] == 'w'
                i -= 1
            end
            c += 2
        elseif dir[c] == 'n'
            j -= 1
            if dir[c+1] == 'e'
                i += 1
            end
            c += 2
        else
            @error "unexpected direction $(dir[c]) at $c"
        end
    end
    return j,i
end
@testset "tile" begin
    @test tile("nwwswee") == (0,0)
end


function extents(io::IO)::Tuple{Int,Int,Int,Int}
    j0, j1, i0, i1 = 0,0,0,0
    for line in eachline(io)
        j, i = tile(line)
        j0 = j < j0 ? j : j0
        j1 = j > j1 ? j : j1
        i0 = i < i0 ? i : i0
        i1 = j > i1 ? i : i1
    end
    return j0, j1, i0, i1
end

struct Floor
    map::BitArray{2}
    mj::Int
    mi::Int
end
Base.getindex(f::Floor, j::Int64, i::Int64) = f.map[j-f.mj+1, i-f.mi+1]
Base.setindex!(f::Floor, v::Bool, j::Int64, i::Int64) = f.map[j-f.mj+1, i-f.mi+1]=v
function Base.show(io::IO, x::Floor)
    r,c = size(x.map)
    for j in 1:r
        write(io, repeat(' ', j-1))
        for i in 1:c
            write(io, x.map[j,i] ? "# " : ". ")
        end
        write(io,"\n")
    end
end

function makefloor(io::IO)::Floor
    j0, j1, i0, i1 = extents(io)
    f = Floor(falses(j1-j0+5, i1-i0+5), j0-2, i0-2)
    seek(io,0)
    for line in eachline(io)
        j,i = tile(line)
        f[j, i] = !f[j,i]
    end
    return f
end
@testset "makefloor" begin
    f = makefloor(IOBuffer(testinput))
    @test count(f.map) == 10
end

function part1()
    open("input/day24.txt") do io
        f = makefloor(io)
        println(count(f.map))
    end
end

const neighbors = [i!=j for j in 1:3, i in 1:3]

function step(f::Floor)::Floor
    r,c = size(f.map)
    g = Floor(falses(r+2,c+2), f.mj-1, f.mi-1)
    for i in 2:c-1
        for j in 2:r-1
            n=count(f.map[j-1:j+1, i-1:i+1][neighbors])
            if (f.map[j,i] && n in 1:2) ||
                (!f.map[j,i] && n == 2)
                g.map[j+1,i+1] = true
            end
        end
    end
    return g
end
@testset "stepfloor" begin
    f = makefloor(IOBuffer(testinput))
    @debug f
    f = step(f)
    @debug f
    @test count(f.map) == 15
    f = step(f)
    @debug f
    @test count(f.map) == 12
    f = step(f)
    @debug f
    @test count(f.map) == 25
end

function part2()
    open("input/day24.txt") do io
        f = makefloor(io)
        for i in 1:100
            f = step(f)
        end
        println(count(f.map))
    end
end
