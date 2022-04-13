local exports = {}

function exports.arrayBind(arr1)
  return function(k)
    local arr3 = {}
    local n = 0
    for i = 1, arr1.n do
      local arr2 = k(arr1[i])
      for j = 1, arr2.n do
        n = n + 1
        arr3[n] = arr2[j]
      end
    end
    arr3.n = n
    return arr3
  end
end

return exports