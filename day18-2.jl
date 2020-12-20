using Test

@enum State scanning in_paren

mutable struct Node
    expression::AbstractString
    left::Union{Node, Nothing}
    right::Union{Node, Nothing}
    op::Union{Function, Nothing}
end

Base.show(io::IO, n::Node) = print(io, n.expression)

function Node(exp::AbstractString)
    exp = strip(exp)
    n = Node(exp, nothing, nothing, nothing)
    ilastop = 0
    state = scanning
    depth = 0
    for i in 1:length(exp)
        if state == scanning
            if exp[i] == '*'
                n.op = (*)
                n.left = Node(exp[1:i-1])
                n.right = Node(exp[i+1:end])
                break
            elseif exp[i] == '+'
                ilastop = i
            elseif exp[i] == '('
                state = in_paren
                depth = 1
            elseif exp[i] == ' '
                continue
            end
        elseif state == in_paren
            if exp[i] == '('
                depth += 1
            elseif exp[i] == ')'
                depth -= 1
                if depth == 0
                    n.left = Node(exp[2:i-1])
                    state = scanning
                end
            else
                continue
            end
        end
    end
    if isnothing(n.op)
        if ilastop > 0 && exp[ilastop] == '+'
            n.op = (+)
            n.left = Node(exp[1:ilastop-1])
            n.right = Node(exp[ilastop+1:end])
        elseif !isnothing(n.left) && isnothing(n.right)
            # fully enclosed paren expression; promote contents
            n = n.left
        end
    end
    @debug "Node" exp n.op n.left n.right
    if !isnothing(n.op) && (isnothing(n.left) || isnothing(n.right))
        @warn "misformed node" exp n.op n.left n.right
    end
    return n
end
@testset "node parsing" begin
    @test Node("1 * 2").op == (*)
    @test Node("1 + 2").op == (+)
    @test Node("1 + (2 * 3) + (4 * (5 + 6))").op == (+)
end

function eval(n::Node)::Int
    if isnothing(n.op) && isnothing(n.left) && isnothing(n.right)
        parse(Int, n.expression)
    else
        n.op(eval(n.left), eval(n.right))
    end
end
@testset "node eval" begin
    @test eval(Node("1 + 2")) == 3
    @test eval(Node("1 + 2 * 3 + 4 * 5 + 6")) == 231
    @test eval(Node("1 + (2 * 3) + (4 * (5 + 6))")) == 51
    @test eval(Node("2 * 3 + (4 * 5)")) == 46
    @test eval(Node("5 + (8 * 3 + 9 + 3 * 4 * 3)")) == 1445
    @test eval(Node("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")) == 669060
    @test eval(Node("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) == 23340
    @test eval(Node("(2 + 6 + 3 + 8 * 4) + (3 + 4) + 5 * 9")) == 792
end

function part2()
    eachline("input/day18.txt") .|> Node .|> eval |> sum |> println
end
@time part2()
