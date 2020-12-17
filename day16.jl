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

function invalidfields(tx::Ticket, rules::Vector{Rule})::Ticket
    filter(x->!any([(x in y.range) for y in rules]), tx)
end

function invalidfields(inp::Input)::Ticket
    invalid = Ticket()
    for tx in inp.others
        inv = invalidfields(tx, inp.rules)
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

const testinput2 = b"""
    class: 0-1 or 4-19
    row: 0-5 or 8-19
    seat: 0-13 or 16-19

    your ticket:
    11,12,13

    nearby tickets:
    3,9,18
    15,1,5
    5,14,9
    """

function filterothers!(inp::Input)
    inp.others = filter(x->length(invalidfields(x, inp.rules))==0, inp.others)
    @debug "filtered others" inp.others
end
@testset "filterothers" begin
    inp = readinput(IOBuffer(testinput))
    @test length(inp.others) == 4
    filterothers!(inp)
    @test length(inp.others) == 1
end

function fields(inp::Input)::Vector{String}
    allfields = Set(map(x->x.field, inp.rules))
    pos = [copy(allfields) for _ in inp.ticket]
    known = ["" for _ in inp.ticket]
    @debug "all possible fields" pos
    # remove fields that don't meet rules
    for tx in [inp.others; inp.ticket]
        for i in 1:length(tx)
            pos[i] = pos[i] ∩ Set(map(x->x.field, filter(x->(tx[i] in x.range), inp.rules)))
        end
    end
    @debug "after applying rules" pos
    # deduce remaining
    while any(isempty, known)
        before = count(isempty, known)
        # only one possibility in a location
        for i in 1:length(pos)
            if length(pos[i])==1
                known[i] = first(pos[i])
                for j in 1:length(pos)
                    setdiff!(pos[j],known[i])
                end
                @debug "found one possibility in location $i" pos known
            end
        end
        # only one location with the field
        for f in allfields
            if count(x->(f in x), pos) == 1
                i = findfirst(x->(f in x), pos)
                known[i] = f
                pos[i] = Set()
                @debug "found one location with field $f" pos known
            end
        end
        # only one location left unknown
        if count(isempty, known) == 1
            last = first(setdiff(allfields, Set(filter(!isempty, known))))
            known[findfirst(isempty, known)] = last
            @debug "only one location with field $last" pos known
        end
        if count(isempty, known) == before
            @error "unable to deduce more locations!"
            break
        end
    end
    @info known
    return known
end
@testset "fields" begin
    inp = readinput(IOBuffer(testinput2))
    @test fields(inp) == ["row", "class", "seat"]
end

function part2()
    inp = readinput("input/day16.txt")
    filterothers!(inp)
    inp.ticket[[startswith(x,"departure") for x in fields(inp)]] |> prod |> println
end
part2()
