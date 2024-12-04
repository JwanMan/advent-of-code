local input = {}

for line in io.lines() do
    local row = {}
    for i = 1,#line do
        -- X,M,A,S --> 1,2,3,4
        row[i] = string.find('XMAS', string.sub(line, i, i)) or 0
    end
    table.insert(input, row)
end

function compute_bounds (dx, size)
    return math.max(0, -3*dx)+1, math.min(size, size-3*dx)
end

function input:_has_xmas (i, j, di, dj)
    for x = 0,3 do
        if self[i+x*di][j+x*dj] ~= x+1 then
            return false
        end
    end
    return true
end

function input:part1 ()
    local m = #self
    local n = 0
    if m ~= 0 then n = #self[1] end
    local sum = 0

    for di = -1,1 do
        local istart, iend = compute_bounds(di, m)
        for dj = -1,1 do
            if di ~= 0 or dj ~= 0 then
                local jstart, jend = compute_bounds(dj, n)
                for i = istart,iend do
                    for j = jstart,jend do
                        if self:_has_xmas(i, j, di, dj) then
                            sum = sum + 1
                        end
                    end
                end
            end
        end
    end
    print(sum)
end

function input:_has_x_mas (i, j)
    if self[i][j] ~= 3 then return false end
    local nm = 0
    for ci = i-1,i+1,2 do
        for cj = j-1,j+1,2 do
            if self[ci][cj] == 2 then
                nm = nm + 1
                if nm > 2 then return false end
            elseif self[ci][cj] ~= 4 then
                return false
            end
        end
    end
    return nm == 2 and self[i-1][j-1] ~= self[i+1][j+1]
end

function input:part2 ()
    local m = #self
    local n = 0
    if m ~= 0 then n = #self[1] end
    local sum = 0
    for i = 2,m-1 do
        for j = 2,n-1 do
            if self:_has_x_mas(i, j) then sum = sum + 1 end
        end
    end
    print(sum)
end

return input
