local exports = {}

function gord(lt)
  return function(eq)
    return function(gt)
      return function(a)
        return function(b)
          if a < b then
            return lt
          elseif a > b then
            return eq
          else
            return gt
          end
        end
      end
    end
  end
end

exports.ordBooleanImpl = gord
exports.ordIntImpl = gord
exports.ordNumberImpl = gord
exports.ordCharImpl = gord
exports.ordStringImpl = gord

function exports.ordArrayImpl(f)
  return function(as)
    return function(bs)
      local n
      if as.n < bs.n then
        n = as.n
      else
        n = bs.n
      for i = 1, n do
        local a = as[i]
        local b = bs[i]
        local o = f(a)(b)
        if o ~= 0 then
          return o
        end
      end
      if as.n == bs.n then
        return 0
      elseif as.n > bs.n then
        return -1
      else
        return 1
      end
    end
  end
end

return exports