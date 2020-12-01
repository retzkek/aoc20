using Test

function main(ns::Array{Int,1})
    sums = ns .+ transpose(ns)
    prods = ns .* transpose(ns)
    mask = sums .== 2020
    maximum(prods[mask])
end

@test main([1721,979,366,299,675,1456]) == 514579

open("input/day01.txt") do f
    println(main(map(x->parse(Int,x), readlines(f))))
end
