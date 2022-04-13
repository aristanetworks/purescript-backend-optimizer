local exports = {}

function exports.pureE(a)
  return function()
    return a
  end
end

function exports.bindE(a)
  return function(k)
    return function()
      return k(a())()
    end
  end
end

function exports.untilE(eff)
  return function()
    while true do
      local res = eff()
      if res then
        return
      end
    end
  end
end

function exports.whileE(cond)
  return function(eff)
    return function()
      while cond() do
        eff()
      end
      return
    end
  end
end

function exports.forE(i)
  return function(j)
    return function(k)
      return function()
        for n = i, j do
          k(n)()
        end
        return
      end
    end
  end
end

function exports.foreachE(arr)
  return function(k)
    return function()
      for i, v = _PS.array_pairs(arr) do
        k(v)()
      end
    end
  end
end

return exports