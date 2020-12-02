using Test

function isvalid(line)
    m = match(r"(\d+)-(\d+) (\w): (\w+)",line)
    @assert !isnothing(m)
    (b,e,c,p) = m.captures
    parse(Int,b) <= length(filter(x->x==c[1], p)) <= parse(Int,e)
end

@test isvalid("1-3 a: abcde")
@test !isvalid("1-3 b: cdefg")
@test isvalid("2-9 c: ccccccccc")

function main()
    open("input/day02.txt") do f
        println(length(filter(x->x,(map(isvalid, readlines(f))))))
    end
end

main()
