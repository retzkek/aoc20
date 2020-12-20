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
matcher(r::Rules, ex::AbstractString) = Regex(string("^", expand(r,ex), "\$"))
matcher(r::Rules) = matcher(r, "0")
@testset "expand" begin
    r = readrules(IOBuffer(testrules))
    @test expand(r,"1") == "a"
    @test expand(r,"3") == "b"
    @test expand(r,"2") == "((ab)|(ba))"
    @test expand(r,"0") == "a((ab)|(ba))"
    rx = matcher(r)
    @test !isnothing(match(rx,"aab"))
    @test !isnothing(match(rx,"aba"))
    r = readrules(IOBuffer(testrules1))
    rx = matcher(r)
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
    rx = matcher(r)
    count([!isnothing(match(rx,line)) for line in eachline(io)])
end
@test checkinput(IOBuffer(testinput)) == 2

function part1()
    open("input/day19.txt") do io
        checkinput(io) |> println
    end
end

const testinput2 = b"""
    42: 9 14 | 10 1
    9: 14 27 | 1 26
    10: 23 14 | 28 1
    1: "a"
    11: 42 31
    5: 1 14 | 15 1
    19: 14 1 | 14 14
    12: 24 14 | 19 1
    16: 15 1 | 14 14
    31: 14 17 | 1 13
    6: 14 14 | 1 14
    2: 1 24 | 14 4
    0: 8 11
    13: 14 3 | 1 12
    15: 1 | 14
    17: 14 2 | 1 7
    23: 25 1 | 22 14
    28: 16 1
    4: 1 1
    20: 14 14 | 1 15
    3: 5 14 | 16 1
    27: 1 6 | 14 18
    14: "b"
    21: 14 1 | 1 14
    25: 1 1 | 1 14
    22: 14 14
    8: 42
    26: 14 22 | 1 20
    18: 15 15
    7: 14 5 | 1 21
    24: 14 1

    abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
    bbabbbbaabaabba
    babbbbaabbbbbabbbbbbaabaaabaaa
    aaabbbbbbaaaabaababaabababbabaaabbababababaaa
    bbbbbbbaaaabbbbaaabbabaaa
    bbbababbbbaaaaaaaabbababaaababaabab
    ababaaaaaabaaab
    ababaaaaabbbaba
    baabbaaaabbaaaababbaababb
    abbbbabbbbaaaababbbbbbaaaababb
    aaaaabbaabaaaaababaa
    aaaabbaaaabbaaa
    aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
    babaaabbbaaabaababbaabababaaab
    aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
    """

function checkinput2(io::IO)::Int
    zs = ["8 11"]
    for i in 1:3
        zs= [zs;
             map(z->replace(z, "8"=>"42 8"),zs);
             map(z->replace(z, "11"=>"42 11 31"),zs)]
    end
    r = readrules(io)
    rx = map(x->matcher(r,x), zs)
    count([any(x->!isnothing(match(x,line)), rx) for line in eachline(io)])
end
@test checkinput2(IOBuffer(testinput2)) == 12

function part2()
    open("input/day19.txt") do io
        checkinput2(io) |> println
    end
end
