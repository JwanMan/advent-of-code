local function parse_instruction (x, y, dont)
    if dont then return 'dont'
    elseif x then return {'mul', tonumber(x), tonumber(y)}
    else return 'do'
    end
end

local function input ()
    local line = ''
    local start, next_mul, next_do = 1, 0, 0
    return function ()
        while true do
            if next_mul and next_mul < start then
                next_mul = string.find(line, 'mul', start)
            end
            if next_do and next_do < start then
                next_do = string.find(line, 'do', start)
            end
            if next_mul and (not next_do or next_mul < next_do) then
                start = next_mul + 1
                local x, y = string.match(line, '^mul%((%d+),(%d+)%)', next_mul)
                if x then return 'mul', tonumber(x), tonumber(y) end
            elseif next_do and (not next_mul or next_do < next_mul) then
                start = next_do + 1
                if string.match(line, '^do%(%)', next_do) then
                    return 'do'
                elseif string.match(line, "^don't%(%)", next_do) then
                    return 'dont'
                end
            elseif not next_mul and not next_do then
                line = io.read '*l'
                if not line then return end
                start, next_mul, next_do = 1, 0, 0
            end
        end
    end
end

local funcs = {reader = input()}

function funcs:part1 ()
    local sum = 0
    for op, arg1, arg2 in self.reader do
        if op == 'mul' then sum = sum + arg1 * arg2 end
    end
    print(sum)
end

function funcs:part2()
    local sum = 0
    local enabled = true
    for op, arg1, arg2 in self.reader do
        print(op, arg1, arg2)
        if op == 'do' then
            enabled = true
        elseif op == 'dont' then
            enabled = false
        elseif op == 'mul' then
            if enabled then sum = sum + arg1 * arg2 end
        end
    end
    print(sum)
end

return funcs
