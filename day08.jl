using Test

const testinput = """
    nop +0
    acc +1
    jmp +4
    acc +3
    jmp -3
    acc -99
    acc +1
    jmp -4
    acc +6"""

mutable struct Instruction
    operation::String
    offset::Int
    execount::Int
end

function readcode(in)
    map(x->(match(r"(\w{3}) ([+-]\d+)", x).captures |>
            y->Instruction(y[1], parse(Int,y[2]), 0)),
        eachline(in))
end
@debug "testinput" readcode(IOBuffer(testinput))

mutable struct Program
    code::Array{Instruction,1}
    addr::Int
    acc::Int
end

function copy(p::Program)
    Program(deepcopy(p.code), p.addr, p.acc)
end

loadprogram(in) = Program(readcode(in),1,0)

function runprogram(program::Program)
    while true
        if program.addr > length(program.code)
            return :halt, program.acc
        end

        instr = program.code[program.addr]
        if instr.execount > 0
            return :loop,  program.acc
        end

        instr.execount += 1
        if instr.operation == "nop"
            program.addr += 1
        elseif instr.operation == "acc"
            program.acc += instr.offset
            program.addr += 1
        elseif instr.operation == "jmp"
            program.addr += instr.offset
        else
            throw(ErrorException("unknown operation $(instr.operation)"))
        end
    end
    # how did I get here?
    (:error, program.acc)
end
@test loadprogram(IOBuffer(testinput)) |> runprogram == (:loop, 5)

function fixprogram(program::Program)
    orig = copy(program)
    for i in 1:length(program.code)
        if program.code[i].operation == "nop"
            program.code[i].operation = "jmp"
        elseif program.code[i].operation == "jmp"
            program.code[i].operation = "nop"
        end
        exit, acc = runprogram(program)
        @debug "fixprogram" program.code exit acc
        if exit == :halt
            return acc
        end
        program = copy(orig)
    end
    -1
end
@test loadprogram(IOBuffer(testinput)) |> fixprogram == 8

function main()
    loadprogram("input/day08.txt") |> runprogram |> println
    loadprogram("input/day08.txt") |> fixprogram |> println
end

main()
