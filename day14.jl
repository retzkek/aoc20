using Test

testinput  = """
    mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    mem[8] = 11
    mem[7] = 101
    mem[8] = 0
    """

function readinput(io)
    mask = repeat("X",36)
    mem = Dict{Int, Int}()
    for line in eachline(io)
        tok = split(line)
        @assert length(tok) == 3
        if tok[1] == "mask"
            mask = tok[3]
            @assert length(mask) == 36
        else
            m = match(r"mem\[(\d+)\]", tok[1])
            @assert !isnothing(m)
            addr = parse(Int, m[1])
            nstr = bitstring(parse(Int64, tok[3]))[64-36+1:end]
            fstr = join([mask[x] != 'X' ? mask[x] : nstr[x] for x in 1:36])
            @debug "mask" mask nstr fstr
            mem[addr] = parse(Int, fstr, base=2)
        end
    end
    return mem
end

@testset "readinput" begin
    mem = readinput(IOBuffer(testinput))
    @test mem[7] == 101
    @test mem[8] == 64
end

function part1()
    mem = readinput("input/day14.txt")
    println(sum(values(mem)))
end
part1()
