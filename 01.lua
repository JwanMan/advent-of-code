function lines ()
    local x = io.read("*n")
    local y = io.read("*n")
    return x, y
end

local input = {{}, {}}

for x, y in lines do
    table.insert(input[1], x)
    table.insert(input[2], y)
end

function input:part1()
    table.sort(self[1])
    table.sort(self[2])

    local sum = 0
    for i = 1,#self[1] do
        sum = sum + math.abs(self[1][i] - self[2][i])
    end

    print(sum)
end

function input:part2()
    local dict = {}
    for _, val in ipairs(self[2]) do
        dict[val] = (dict[val] or 0) + 1
    end

    local sum = 0
    for _, val in ipairs(self[1]) do
        if dict[val] then sum = sum + val * dict[val] end
    end
    print(sum)
end

return input
