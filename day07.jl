using Test

testinput = b"""
    light red bags contain 1 bright white bag, 2 muted yellow bags.
    dark orange bags contain 3 bright white bags, 4 muted yellow bags.
    bright white bags contain 1 shiny gold bag.
    muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
    shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
    dark olive bags contain 3 faded blue bags, 4 dotted black bags.
    vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    faded blue bags contain no other bags.
    dotted black bags contain no other bags.
    """

const Contents = Array{Tuple{Int,String},1}

function parserule(s)
    r = findfirst("bags contain",s)
    bag = s[1:first(r)-2]
    rest = chomp(s[last(r)+1:end])
    if rest == " no other bags."
        return (bag, Contents())
    end
    contents::Contents = split(rest,",") .|>
        function (x)
            x = strip(x)
            m = match(r"(\d+) (.+) bags?",x)
            if isnothing(m) return () end
            n,b = m.captures
            (parse(Int,n), b)
        end
    (bag, contents)
end

@testset "parserule" begin
    @test parserule("light red bags contain 1 bright white bag, 2 muted yellow bags.") ==
        ("light red", [(1, "bright white"), (2, "muted yellow")])
    @test parserule("bright white bags contain 1 shiny gold bag.") ==
        ("bright white", [(1, "shiny gold")])
    @test parserule("faded blue bags contain no other bags.") ==
        ("faded blue", [])
end

function graphrules(rules)
    println("digraph {")
    for b in rules
        for c in b[2]
            println("\"",b[1],"\" -> \"",c[2],"\"")
        end
    end
    println("}")
end

function whatcontains(rules, bag)
    cs = map(x->x[1],filter(x->any(y->(y[2] == bag), x[2]), rules))
    if length(cs) == 0
        Array{String,1}[]
    else
        union(cs, map(x->whatcontains(rules, x),cs)...)
    end
end

@testset "whatcontains" begin
    r = IOBuffer(testinput) |> eachline .|> parserule
    @debug "test rules" r
    @test whatcontains(r, "dark orange") == []
    @test whatcontains(r, "bright white") == ["light red","dark orange"]
    @test length(whatcontains(r, "shiny gold")) == 4
end

function howmany(rules, bag)
    _,cs = rules[findfirst(x->x[1]==bag, rules)]
    @debug "cs" cs
    if length(cs) == 0
        0
    else
        sum(map(x->x[1]*(1+howmany(rules, x[2])), cs))
    end
end

@testset "howmany" begin
    r = IOBuffer(testinput) |> eachline .|> parserule
    @test howmany(r, "shiny gold") == 32
end

function main()
    r = eachline("input/day07.txt") .|> parserule
    @debug "rules" r
    println("part 1: ",length(whatcontains(r, "shiny gold")))
    println("part 2: ",howmany(r, "shiny gold"))
end

main()

function graph()
    r = eachline("input/day07.txt") .|> parserule
    graphrules(r)
end
