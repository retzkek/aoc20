using Test

const testinput = b"""
    .#.
    ..#
    ###
    """

Space = BitArray{4}

function initspace(in::IO, iter::Int)::Space
    # determine dimensions
    r = c = 0
    for line in eachline(in)
        r += 1
        if c == 0
            c = length(line)
        else
            @assert length(line) == c "number of columns in input changes at line $r"
        end
    end
    # allocation and read initial state
    seek(in,0)
    space = falses(r+iter*2,c+iter*2,1+iter*2,1+iter*2)
    j = iter
    for line in eachline(in)
        j += 1
        for i in 1:c
            space[j,i+iter,iter+1,iter+1] = line[i] == '#'
        end
    end
    @debug "initial state" space
    return space
end
@testset "initspace" begin
    s = initspace(IOBuffer(testinput),1)
    @test size(s) == (5,5,3,3)
    @test s[2:4,2:4,2,2] == [0 1 0; 0 0 1; 1 1 1]
end

function update(space::Space)::Space
    (r, c, l, w) = size(space)
    new = falses(r,c,l,w)
    for q in 1:w
        for k in 1:l
            for i in 1:c
                for j in 1:r
                    neighbors = @view space[max(j-1,1):min(j+1,r),
                                            max(i-1,1):min(i+1,c),
                                            max(k-1,1):min(k+1,l),
                                            max(q-1,1):min(q+1,w)]
                    n = count(neighbors)
                    if space[j,i,k,q] && n in (3,4)
                        # If a cube is active and exactly 2 or 3 of its neighbors
                        # are also active, the cube remains active. Otherwise, the
                        # cube becomes inactive. NB: n includes itself
                        new[j,i,k,q] = true
                    elseif !space[j,i,k,q] && n == 3
                        # If a cube is inactive but exactly 3 of its neighbors are
                        # active, the cube becomes active. Otherwise, the cube
                        # remains inactive.
                        new[j,i,k,q] = true
                    end
                end
            end
        end
    end
    @debug "updated" new
    return new
end
@testset "update" begin
    s = initspace(IOBuffer(testinput),1)
    s = update(s)
    @test s[3:5, 2:4, 1, 2] == [1 0 0; 0 0 1; 0 1 0]
    @test s[3:5, 2:4, 2, 2] == [1 0 1; 0 1 1; 0 1 0]
    @test s[3:5, 2:4, 3, 2] == [1 0 0; 0 0 1; 0 1 0]
    s = initspace(IOBuffer(testinput),6)
    for i in 1:6
        s = update(s)
    end
    @test count(s) == 848
end

function part2()
    open("input/day17.txt", "r") do io
        s = initspace(io, 6)
        for i in 1:6
            s = update(s)
        end
        count(s) |> println
    end
end
part2()
