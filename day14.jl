using Test

testinput  = """
    mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
    mem[8] = 11
    mem[7] = 101
    mem[8] = 0
    """

function setmaskedvalue(mem::Dict{Int,Int}, mask::AbstractString, addr::Int, value::Int)
    nstr = bitstring(value)[64-36+1:end]
    fstr = join([mask[x] != 'X' ? mask[x] : nstr[x] for x in 1:36])
    @debug "mask" mask nstr fstr
    mem[addr] = parse(Int, fstr, base=2)
end

function maskaddr(addr, mask)
    if length(addr) == 0 || length(mask) == 0
        [""]
    else
        f(x) = map(y->join([x, y]), maskaddr(addr[2:end], mask[2:end]))
        if mask[1] == '0'
            f(addr[1])
        elseif mask[1] == '1'
            f('1')
        else
            [f('0'); f('1')]
        end
    end
end
@test maskaddr("101010","X1001X") == ["011010","011011","111010","111011"]


function setmaskedaddr(mem::Dict{Int,Int}, mask::AbstractString, addr::Int, value::Int)
    bstr = bitstring(addr)[64-36+1:end]
    for faddr in maskaddr(bstr, mask)
        @debug "mask" mask addr faddr
        mem[parse(Int, faddr, base=2)] = value
    end
end

function readinput(io, maskfn)
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
            maskfn(mem, mask, parse(Int, m[1]), parse(Int, tok[3]))
        end
    end
    return mem
end

@testset "readinput" begin
    mem = readinput(IOBuffer(testinput), setmaskedvalue)
    @test mem[7] == 101
    @test mem[8] == 64
end

function part1()
    mem = readinput("input/day14.txt", setmaskedvalue)
    println(sum(values(mem)))
end
part1()

function part2()
    mem = readinput("input/day14.txt", setmaskedaddr)
    println(sum(values(mem)))
end
part2()
