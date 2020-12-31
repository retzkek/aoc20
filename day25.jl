using Test

const testinput = b"""
    5764801
    17807724
    """

transform(n, subj) = (n*subj)%20201227

function loops(key, subject=7)
    n = subject
    i = 1
    while n != key
        n = transform(n, subject)
        i += 1
    end
    return i
end
@testset "loops" begin
    @test loops(5764801) == 8
    @test loops(17807724) == 11
end

function encrypt(key, loops)
    n = 1
    for i in 1:loops
        n = transform(n, key)
    end
    return n
end
@testset "encrypt" begin
    @test encrypt(5764801, 11) == 14897079
    @test encrypt(17807724, 8) == 14897079
end

function part1()
    open("input/day25.txt") do io
        door = parse(Int, readline(io))
        card = parse(Int, readline(io))
        secret = loops(door)
        println(encrypt(card, secret))
    end
end
