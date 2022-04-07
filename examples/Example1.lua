local function bind(a)
  return function (k)
    return function()
      return k(a())()
    end
  end
end

local function pure(a)
  return function()
    return a
  end
end

local function log(str)
  return function()
    print(str)
    return unit
  end
end

local unit = {"Unit"}

return {
  bind = bind,
  pure = pure,
  unit = unit,
  log = log
}