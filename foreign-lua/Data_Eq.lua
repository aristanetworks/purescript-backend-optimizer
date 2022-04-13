local exports = {}

local function geq(a)
  return function(b)
    return a == b
  end
end

exports.eqBooleanImpl = geq
exports.eqIntImpl = geq
exports.eqNumberImpl = geq
exports.eqCharImpl = geq
exports.eqStringImpl = geq

function exports.eqArrayImpl(eq)
  return function(a)
    return function(b)
      if a.n ~= b.n then
        return false
      end
      for i = 1, a.n then
        if not eq(a)(b) then
          return false
        end
      end
      return true
    end
  end
end

return exports