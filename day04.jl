using Test

testdata = b"""
    ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
    byr:1937 iyr:2017 cid:147 hgt:183cm

    iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
    hcl:#cfa07d byr:1929

    hcl:#ae17e1 iyr:2013
    eyr:2024
    ecl:brn pid:760753108 byr:1931
    hgt:179cm

    hcl:#cfa07d eyr:2025 pid:166559648
    iyr:2011 ecl:brn hgt:59in
    """

function parsefield(s)
    m = match(r"(\w+):(\S+)", s)
    if isnothing(m)
        return ()
    end
    return m[1],m[2]
end
@test parsefield("ecl:gry") == ("ecl","gry")
@test parsefield("") == ()

function parseline(line)
    [parsefield(x) for x in split(line)]
end
@test parseline("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd") ==
[("ecl","gry"),("pid","860033327"),("eyr","2020"),("hcl","#fffffd")]

function read_passports(in)
    passports = []
    passport = Dict()
    for line in eachline(in)
        fields = parseline(line)
        if length(fields) == 0 && length(passport) > 0
            push!(passports,passport)
            passport=Dict()
        else
            map(x->passport[x[1]]=x[2], fields)
        end
    end
    if length(passport) > 0
        push!(passports,passport)
    end
    passports
end
@test length(read_passports(IOBuffer(testdata))) == 4

function valid(p)
    required = ("byr","iyr","eyr","hgt","hcl","ecl","pid")
    for f in required
        if !haskey(p,f)
            return false
        end
    end
    true
end

@test read_passports(IOBuffer(testdata)) .|> valid |> sum == 2

function main()
    read_passports("input/day04.txt") .|>  valid |> sum
end

println(main())
