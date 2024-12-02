local Report = {}
Report.__index = Report

function Report:new (values)
    if #values == 0 then error "Cannot construct an empty report." end
    setmetatable(values, Report)
    return values
end

local function valid_jump (start, finish)
    local diff = finish - start
    return diff >= 1 and diff <= 3
end

function Report:is_safe_factor (mult, allow_dampen)
    local prev = self[1] * mult
    for i = 2,#self do
        local cur = self[i] * mult

        if valid_jump(prev, cur) then
            prev = cur
        elseif not allow_dampen then
            return false
        else
            local remove_cur = i == #self or valid_jump(prev, self[i+1] * mult)

            if not remove_cur then
                local remove_prev = i == 2 or valid_jump(self[i-2] * mult, cur)
                if not remove_prev then return false end
                prev = cur
            end

            allow_dampen = false
        end
    end
    return true
end

function Report:is_safe (allow_dampen)
    return self:is_safe_factor(1, allow_dampen) or
        self:is_safe_factor(-1, allow_dampen)
end

local input = {}

for line in function () return io.read("*l") end do
    local parsed = {}
    for v in line:gmatch("%d+") do
        table.insert(parsed, tonumber(v))
    end
    table.insert(input, Report:new(parsed))
end

function input:sum_safe (allow_dampen)
    local sum = 0
    for _, x in ipairs(input) do
        if x:is_safe(allow_dampen) then sum = sum + 1 end
    end
    return sum
end

function input:part1 ()
    print(self:sum_safe(false))
end

function input:part2 ()
    print(self:sum_safe(true))
end

return input
