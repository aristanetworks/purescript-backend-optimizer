local exports = {}

function exports.intSub(x)
  return function(y)
    return x - y | 0
  end
end

function exports.numSub(x)
  return function(y)
    return x - y
  end
end

return exports