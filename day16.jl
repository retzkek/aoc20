using Test

const testinput = b"""
    class: 1-3 or 5-7
    row: 6-11 or 33-44
    seat: 13-40 or 45-50

    your ticket:
    7,1,14

    nearby tickets:
    7,3,47
    40,4,50
    55,2,20
    38,6,12
    """

@enum InputState ticket_rules blank_line your_ticket nearby_ticket

struct Rule
    field::AbstractString
    range::AbstractRange{Int}
end

Ticket = Vector{Int}

mutable struct Input
    rules::Vector{Rule}
    ticket::Ticket
    others::Vector{Ticket}
end

Input() = Input(Vector{Rule}(), Ticket(), Vector{Ticket}())

function readinput(in)
    input = Input()
    state = ticket_rules
    for line in eachline(in)
        if line == ""
            state = blank_line
        elseif line == "your ticket:"
            state = your_ticket
        elseif line == "nearby tickets:"
            state = nearby_ticket
        elseif state == ticket_rules
            m = match(r"([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)", line)
            if isnothing(m)
                continue
            end
            ranges = [parse(Int,x) for x in m.captures[2:end]]
            @debug "rule" line m[1] ranges
            push!(input.rules, Rule(m[1], ranges[1]:ranges[2]))
            push!(input.rules, Rule(m[1], ranges[3]:ranges[4]))
        elseif state == your_ticket
            input.ticket = [parse(Int,x) for x in split(line,",")]
            @debug "ticket" line input.ticket
        elseif state == nearby_ticket
            ticket = [parse(Int,x) for x in split(line,",")]
            @debug "other" line ticket
            push!(input.others, ticket)
        end
    end
    return input
end
@testset "readinput" begin
    inp = readinput(IOBuffer(testinput))
    @test length(inp.rules) == 6
    @test inp.ticket == [7,1,14]
    @test length(inp.others) == 4
end

function invalidfields(inp::Input)::Ticket
    invalid = Ticket()
    for tx in inp.others
        inv = filter(x->!any([(x in y.range) for y in inp.rules]), tx)
        @debug "invalid" tx inv
        invalid = [invalid; inv]
    end
    return invalid
end
@testset "invalidfields" begin
    inp = readinput(IOBuffer(testinput))
    @test invalidfields(inp) == [4,55,12]
end

function part1()
    inp = readinput("input/day16.txt")
    invalidfields(inp) |> sum |> println
end
part1()
