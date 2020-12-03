using Test

function parseline(line)
    m = match(r"(\d+)-(\d+) (\w): (\w+)",line)
    @assert !isnothing(m)
    (parse(Int,m[1]), parse(Int,m[2]), m[3][1], m[4])
end
@test parseline("1-3 a: abcde") == (1,3,'a',"abcde")

macro pw_str(line)
    parseline(line)
end
@test pw"1-3 a: abcde" == (1,3,'a',"abcde")

isvalid1(b, e, c, p) = b <= length(filter(x->x==c, p)) <= e
@test isvalid1(pw"1-3 a: abcde"...)
@test !isvalid1(pw"1-3 b: cdefg"...)
@test isvalid1(pw"2-9 c: ccccccccc"...)

isvalid2(b, e, c, p) = (p[b] == c) ⊻ (p[e] == c)
@test isvalid2(pw"1-3 a: abcde"...)
@test !isvalid2(pw"1-3 b: cdefg"...)
@test !isvalid2(pw"2-9 c: ccccccccc"...)

function main(fn)
    println(sum(map(x->fn(parseline(x)...), eachline("input/day02.txt"))))
end

main(isvalid1)
main(isvalid2)
