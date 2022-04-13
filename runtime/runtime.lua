_PS = {}

function _PS.array(...)
  return table.pack(...)
end

function _PS.array_pairs(arr)
  local i = 0
  return function()
    if i < arr.n then
      local j = i
      i = i + 1
      return j, arr[i]
    end
  end
end