using Test

const testrules = b"""
    0: 1 2
    1: "a"
    2: 1 3 | 3 1
    3: "b"
    """

const testrules1 = b"""
    0: 4 1 5
    1: 2 3 | 3 2
    2: 4 4 | 5 5
    3: 4 5 | 5 4
    4: "a"
    5: "b"
    """

const testinput = b"""
    0: 4 1 5
    1: 2 3 | 3 2
    2: 4 4 | 5 5
    3: 4 5 | 5 4
    4: "a"
    5: "b"

    ababbb
    bababa
    abbbab
    aaabbb
    aaaabbb
    """

Rules = Dict{String,String}

function readrules(io::IO)::Rules
    r = Rules()
    while true
        line = readline(io)
        @debug line
        if line == ""
            break
        end
        tok = split(line, ':')
        r[tok[1]] = strip(tok[2])
    end
    return r
end
@debug "readrules" readrules(IOBuffer(testrules))

function expand(r::Rules, ex::AbstractString)::String
    @debug "expand" ex
    m = match(r"\"(\w)\"",ex)
    if !isnothing(m)
        return m[1]
    end
    m = match(r"([\d ]+) \| ([\d ]+)",ex)
    if !isnothing(m)
        return "(($(expand(r, m[1])))|($(expand(r,m[2]))))"
    end
    m = match(r"(\d+ ?)+",ex)
    if !isnothing(m)
        return join(map(x->expand(r,r[x]), split(ex)))
    end
    error("unrecognized expression '$ex'")
end
expand(r::Rules) = string("^", expand(r,"0"), "\$")
@testset "expand" begin
    r = readrules(IOBuffer(testrules))
    @test expand(r,"1") == "a"
    @test expand(r,"3") == "b"
    @test expand(r,"2") == "((ab)|(ba))"
    @test expand(r,"0") == "a((ab)|(ba))"
    @test expand(r) == "^a((ab)|(ba))\$"
    rx = Regex(expand(r))
    @test !isnothing(match(rx,"aab"))
    @test !isnothing(match(rx,"aba"))
    r = readrules(IOBuffer(testrules1))
    rx = Regex(expand(r))
    @test !isnothing(match(rx,"aaaabb"))
    @test !isnothing(match(rx,"aaabab"))
    @test !isnothing(match(rx,"abbabb"))
    @test !isnothing(match(rx,"abbbab"))
    @test !isnothing(match(rx,"aabaab"))
    @test !isnothing(match(rx,"aabbbb"))
    @test !isnothing(match(rx,"abaaab"))
    @test !isnothing(match(rx,"ababbb"))
end

function checkinput(io::IO)::Int
    r = readrules(io)
    rx = Regex(expand(r))
    count([!isnothing(match(rx,line)) for line in eachline(io)])
end
@test checkinput(IOBuffer(testinput)) == 2

function part1()
    open("input/day19.txt") do io
        checkinput(io) |> println
    end
end
