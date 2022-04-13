local exports = {}

function exports.arrayApply(arr1)
  return function(arr2)
    local arr3 = {}
    arr3.n = arr1.n * arr2.n
    for i = 1, arr1.n do
      for j = 1, arr2.n do
        arr3[i * j] = arr1[i](arr2[j])
      end
    end
    return arr3
  end
end

return exports