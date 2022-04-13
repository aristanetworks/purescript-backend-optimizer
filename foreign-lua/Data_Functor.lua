local exports = {}

function exports.arrayMap(f)
  return function(arr1)
    local arr2 = {}
    arr2.n = arr1.n
    for i = 1, arr1.n do
      arr2[i] = f(arr1[i])
    end
    return arr2
  end
end

return exports