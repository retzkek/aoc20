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
isvalid1((b, e, c, p)) = isvalid1(b,e,c,p)
@test isvalid1(pw"1-3 a: abcde")
@test !isvalid1(pw"1-3 b: cdefg")
@test isvalid1(pw"2-9 c: ccccccccc")

isvalid2(b, e, c, p) = (p[b] == c) ⊻ (p[e] == c)
isvalid2((b, e, c, p)) = isvalid2(b,e,c,p)
@test isvalid2(pw"1-3 a: abcde")
@test !isvalid2(pw"1-3 b: cdefg")
@test !isvalid2(pw"2-9 c: ccccccccc")

function main(fn)
    #println(sum(map(x->fn(parseline(x)...), eachline("input/day02.txt"))))
    # That works, but maybe pipes and function composition is clearer?
    eachline("input/day02.txt") .|> fn ∘ parseline |> sum |> println
    # I don't know, too much syntax maybe. Handy for rapid development and
    # debugging though, just build the pipe one part at a time. To debug a step
    # just comment out the rest of the pipe.
end

main(isvalid1)
main(isvalid2)
