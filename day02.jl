using Test

function isvalid1(line)
    m = match(r"(\d+)-(\d+) (\w): (\w+)",line)
    @assert !isnothing(m)
    (b,e,c,p) = m.captures
    parse(Int,b) <= length(filter(x->x==c[1], p)) <= parse(Int,e)
end

@test isvalid1("1-3 a: abcde")
@test !isvalid1("1-3 b: cdefg")
@test isvalid1("2-9 c: ccccccccc")

function isvalid2(line)
    m = match(r"(\d+)-(\d+) (\w): (\w+)",line)
    @assert !isnothing(m)
    (b,e,c,p) = m.captures
    (p[parse(Int,b)] == c[1]) ⊻ (p[parse(Int,e)] == c[1])
end

@test isvalid2("1-3 a: abcde")
@test !isvalid2("1-3 b: cdefg")
@test !isvalid2("2-9 c: ccccccccc")

function main(fn)
    open("input/day02.txt") do f
        println(length(filter(x->x,(map(fn, readlines(f))))))
    end
end

main(isvalid1)
main(isvalid2)
