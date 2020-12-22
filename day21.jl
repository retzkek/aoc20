using Test

const testinput = b"""
    mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    trh fvjkl sbzzf mxmxvkd (contains dairy)
    sqjhc fvjkl (contains soy)
    sqjhc mxmxvkd sbzzf (contains fish)
    """

Aset = Set{AbstractString}
struct Food
    ingredients::Aset
    allergens::Aset
end
Foods = Vector{Food}

function readfoods(in)::Foods
    foods = Foods()
    for line in eachline(in)
        m = match(r"([\w ]+) \(contains ([\w, ]+)\)", line)
        @assert !isnothing(m)
        f = Food(Set(split(m[1])), Set([strip(x) for x in split(m[2],",")]))
        push!(foods,f)
    end
    @debug "readfoods" foods
    foods
end
@test length(readfoods(IOBuffer(testinput))) == 4

function nonallergenic(foods::Foods)::Int
    ingredients = Aset()
    allergens = Aset()
    for f in foods
        ingredients = union!(ingredients, f.ingredients)
        allergens = union!(allergens, f.allergens)
    end
    @debug "unknowns" ingredients allergens
    # map allergen to the ingredient that contains it
    known = Dict{AbstractString,AbstractString}()
    before = length(known)
    while length(allergens) > 0
        # find foods that have the same allergen and ingredient in common
        for a in allergens
            common = copy(ingredients)
            for f in foods
                if a in f.allergens
                    intersect!(common, f.ingredients)
                end
            end
            if length(common) == 1
                known[a] = first(common)
                delete!(allergens,a)
                delete!(ingredients,known[a])
            end
        end
        # find foods that have one ingredient that is not a known allergen and
        # also have an unknown allergen
        for f in foods
            ui = setdiff(f.ingredients, values(known))
            ua = setdiff(f.allergens, keys(known))
            if length(ui) == length(ua) == 1
                known[first(ua)] = first(ui)
                delete!(allergens,first(ua))
                delete!(ingredients,first(ui))
            end
        end
        @debug "known" known
        if length(known) == before
            @warn "unable to deduce any more allergens"
            return 0
        end
    end

    # count non-allergenic ingredients
    sum([length(setdiff(f.ingredients, values(known))) for f in foods])
end
@test nonallergenic(readfoods(IOBuffer(testinput))) == 5

function part1()
    nonallergenic(readfoods("input/day21.txt")) |> println
end
