using Test

testinput = """
    L.LL.LL.LL
    LLLLLLL.LL
    L.L.L..L..
    LLLL.LL.LL
    L.LL.LL.LL
    L.LLLLL.LL
    ..L.L.....
    LLLLLLLLLL
    L.LLLLLL.L
    L.LLLLL.LL
    """

@enum Ferry empty filled floor

function readferry(in)::Array{Ferry,2}
    rows = cols = 0
    for row in eachline(in)
        rows += 1
        if cols > 0 && length(row) != cols
            throw(ErrorException("expected $cols columns in input, got $(length(row)) on line $rows"))
        end
        cols = length(row)
    end
    board = [floor for j in 1:rows, i in 1:cols]
    seek(in,0)

    j = 0
    for line in eachline(in)
        j += 1
        for i in 1:cols
            board[j,i] = line[i] == 'L' ? empty :
                line[i] == '#' ? filled :
                floor
        end
    end
    return board
end
@debug "readferry" readferry(IOBuffer(testinput))

function update(board::Array{Ferry,2})::Tuple{Array{Ferry,2}, Bool}
    @debug "pre-update" board
    (r, c) = size(board)
    new = copy(board)
    for i in 1:c
        for j in 1:r
            v = @view board[max(j-1,1):min(j+1,r), max(i-1,1):min(i+1,c)]
            n = count(x->x==filled, v)
            @debug v n
            if board[j,i] == filled && n > 4
                # If a seat is occupied (#) and four or more seats adjacent to
                # it are also occupied, the seat becomes empty.
                # Note that the seat itself is counted in n.
                new[j,i] = empty
            elseif board[j,i] == empty && n == 0
                # If a seat is empty (L) and there are no occupied seats adjacent
                # to it, the seat becomes occupied.
                new[j,i] = filled
            end
        end
    end
    @debug "post-update" new
    return new, new==board
end
@testset "update board" begin
    board = readferry(IOBuffer(testinput))
    board,same = update(board)
    @test board == Array{Ferry,2}([
        filled  floor   filled  filled  floor   filled  filled  floor   filled  filled
        filled  filled  filled  filled  filled  filled  filled  floor   filled  filled
        filled  floor   filled  floor   filled  floor   floor   filled  floor   floor
        filled  filled  filled  filled  floor   filled  filled  floor   filled  filled
        filled  floor   filled  filled  floor   filled  filled  floor   filled  filled
        filled  floor   filled  filled  filled  filled  filled  floor   filled  filled
        floor   floor   filled  floor   filled  floor   floor   floor   floor   floor
        filled  filled  filled  filled  filled  filled  filled  filled  filled  filled
        filled  floor   filled  filled  filled  filled  filled  filled  floor   filled
        filled  floor   filled  filled  filled  filled  filled  floor   filled  filled])
    @test !same
    board,same = update(board)
    @test board == Array{Ferry,2}([
        filled  floor  empty   empty  floor  empty  filled  floor  filled  filled
        filled  empty  empty   empty  empty  empty  empty   floor  empty   filled
        empty   floor  empty   floor  empty  floor  floor   empty  floor   floor
        filled  empty  empty   empty  floor  empty  empty   floor  empty   filled
        filled  floor  empty   empty  floor  empty  empty   floor  empty   empty
        filled  floor  empty   empty  empty  empty  filled  floor  filled  filled
        floor   floor  empty   floor  empty  floor  floor   floor  floor   floor
        filled  empty  empty   empty  empty  empty  empty   empty  empty   filled
        filled  floor  empty   empty  empty  empty  empty   empty  floor   empty
        filled  floor  filled  empty  empty  empty  empty   floor  filled  filled])
    @test !same
    board,same = update(board)
    @test !same
    board,same = update(board)
    @test !same
    board,same = update(board)
    @test !same
    board,same = update(board)
    @test same
end

function main()
    open("input/day11.txt", "r") do io
        board = readferry(io)
        stable = false
        while !stable
            board, stable = update(board)
        end
        println(count(x->x==filled, board))
    end
end

main()
