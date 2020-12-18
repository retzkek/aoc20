using Test
using Chain

@enum State scanning in_num in_paren

function apply(op, acc, n)
    if isnothing(op)
        n
    else
        op(acc,n)
    end
end

function evalexp(eq::AbstractString)::Int
    @debug eq
    state = scanning
    start = 1
    acc = 0
    op = nothing
    depth = 0
    for i in 1:length(eq)
        if state == scanning
            if isdigit(eq[i])
                @debug "in_num" i
                state = in_num
                start = i
            elseif eq[i] == '('
                @debug "in_paren" i
                state = in_paren
                depth = 1
                start = i
            elseif eq[i] == '+'
                @debug "op" i eq[i]
                op = +
            elseif eq[i] == '*'
                @debug "op" i eq[i]
                op = *
            elseif eq[i] == ' '
                continue
            else
                error("unexpected character '$(eq[i])' while scanning at column $i")
            end
        elseif state == in_num
            if eq[i] in (' ', ')')
                n = parse(Int, eq[start:i-1])
                @debug "end num" i n
                acc = apply(op, acc, n)
                state = scanning
            else
                error("unexpected character '$(eq[i])' while reading number at column $i")
            end
        elseif state == in_paren
            if eq[i] == '('
                depth += 1
            elseif eq[i] == ')'
                depth -= 1
                if depth == 0
                    n = evalexp(eq[start+1:i-1])
                    @debug "end paren" i n
                    acc = apply(op, acc, n)
                    state = scanning
                end
            else
                continue
            end
        end
    end
    if state == in_num
        n = parse(Int, eq[start:end])
        @debug "end num (end eq)" n
        acc = apply(op, acc, n)
    elseif state == in_paren
        error("unterminated parens in '$eq'")
    end
    return acc
end
@testset "evalexp" begin
    @test evalexp("1 + 2") == 3
    @test evalexp("1 + (2 * 3) + (4 * (5 + 6))") == 51
    @test evalexp("2 * 3 + (4 * 5)") == 26
    @test evalexp("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 437
    @test evalexp("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 12240
    @test evalexp("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632
end

function part1()
    eachline("input/day18.txt") .|> evalexp |> sum |> println
end
@time part1()
