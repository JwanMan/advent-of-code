#!/usr/bin/lua

if #arg < 1 or #arg > 2 then
    error ("Syntax: %s <DAY><a|b> [inputfile]"):format(arg[0])
    os.exit(1)
end

local num, letter = arg[1]:match("^(%d+)([ab])$")
if not num then
    error ('Expected <DAY><a|b> as first argument (e.g. "01b"), got: %s'):format(arg[1])
    os.exit(1)
end
num = tonumber(num)

local script = ("%02d.lua"):format(num)
local infile = arg[2] or ("%02d.in"):format(num)
local input_stream = io.open(infile)
if input_stream then
    io.input(input_stream)
else
    print "Reading from standard input."
end

local parsed = loadfile(script)()
if letter == 'a' then
    parsed:part1()
else
    parsed:part2()
end
