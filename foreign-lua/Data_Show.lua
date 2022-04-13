local exports = {}

function exports.showIntImpl(n)
  return tostring(n)
end

function exports.showNumberImpl(n)
  return toString(n)
end

function exports.showCharImpl(ch)
  error("Not implemented: showCharImpl")
end

function exports.showStringImpl(str)
  error("Not implemented: showStringImpl")
end

function exports.showArrayImpl(show)
  return function(arr)
    local buff = "["
    for i, val = _PS.array_pairs(arr) do
      if i == 1 then
        buff = buff .. show(val)
      else
        buff = buff .. ", " .. show (val)
      end
    end
    return buff .. "]"
  end
end

return exports