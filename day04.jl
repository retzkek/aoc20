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

function hasfields(p)
    required = ("byr","iyr","eyr","hgt","hcl","ecl","pid")
    for f in required
        if !haskey(p,f)
            return false
        end
    end
    true
end
@test read_passports(IOBuffer(testdata)) .|> hasfields |> sum == 2

#    byr (Birth Year) - four digits; at least 1920 and at most 2002.
#    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
#    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
#    hgt (Height) - a number followed by either cm or in:
#        If cm, the number must be at least 150 and at most 193.
#        If in, the number must be at least 59 and at most 76.
#    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
#    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
#    pid (Passport ID) - a nine-digit number, including leading zeroes.
#    cid (Country ID) - ignored, missing or not.


valid_byr(p) = 1920 <= parse(Int,p["byr"]) <= 2002
valid_iyr(p) = 2010 <= parse(Int,p["iyr"]) <= 2020
valid_eyr(p) = 2020 <= parse(Int,p["eyr"]) <= 2030

function valid_hgt(p)
    m = match(r"(\d+)(\w{2})", p["hgt"])
    if isnothing(m) return false end
    h = parse(Int, m[1])
    if m[2] == "cm"
        150 <= h <= 193
    elseif m[2] == "in"
        59 <= h <= 76
    else
        false
    end
end
@testset "hgt" begin
    @test valid_hgt(Dict("hgt"=>"60in"))
    @test valid_hgt(Dict("hgt"=>"190cm"))
    @test !valid_hgt(Dict("hgt"=>"190in"))
    @test !valid_hgt(Dict("hgt"=>"190"))
end

valid_hcl(p) = !isnothing(match(r"^#[0-9a-f]{6}$",p["hcl"]))
@testset "hcl" begin
    @test valid_hcl(Dict("hcl"=>"#123abc"))
    @test !valid_hcl(Dict("hcl"=>"#123abz"))
    @test !valid_hcl(Dict("hcl"=>"123abc"))
end

valid_ecl(p) = p["ecl"] in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
@testset "ecl" begin
    @test valid_ecl(Dict("ecl"=>"brn"))
    @test !valid_ecl(Dict("ecl"=>"wat"))
end

valid_pid(p)= !isnothing(match(r"^\d{9}$",p["pid"]))
@testset "pid" begin
    @test valid_pid(Dict("pid"=>"000000001"))
    @test !valid_pid(Dict("pid"=>"0123456789"))
end

valid(p) = hasfields(p) && valid_byr(p) && valid_iyr(p) && valid_eyr(p) &&
    valid_hgt(p) && valid_hcl(p) && valid_ecl(p) && valid_pid(p)

@testset "passports" begin
    testinvalid = b"""
        eyr:1972 cid:100
        hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

        iyr:2019
        hcl:#602927 eyr:1967 hgt:170cm
        ecl:grn pid:012533040 byr:1946

        hcl:dab227 iyr:2012
        ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

        hgt:59cm ecl:zzz
        eyr:2038 hcl:74454a iyr:2023
        pid:3556412378 byr:2007
        """
    @test read_passports(IOBuffer(testinvalid)) .|> valid |> sum == 0
    testvalid = b"""
        pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
        hcl:#623a2f

        eyr:2029 ecl:blu cid:129 byr:1989
        iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

        hcl:#888785
        hgt:164cm byr:2001 iyr:2015 cid:88
        pid:545766238 ecl:hzl
        eyr:2022

        iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
        """
    @test read_passports(IOBuffer(testvalid)) .|> valid |> sum == 4
end

function main(validator)
    read_passports("input/day04.txt") .|>  validator |> sum
end

println(main(hasfields))
println(main(valid))
