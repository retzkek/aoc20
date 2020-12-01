using Test

function main(ns::Array{Int,1})
    two = transpose(ns)
    three = reshape(ns,1,1,length(ns))
    sums = ns .+ two .+ three
    prods = ns .* two .* three
    mask = sums .== 2020
    maximum(prods[mask])
end

@test main([1721,979,366,299,675,1456]) == 241861950

open("input/day01.txt") do f
    println(main(map(x->parse(Int,x), readlines(f))))
end
