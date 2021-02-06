local function swapkv(tbl)
  local res = {}
  for k, v in pairs(tbl) do
    res[v] = k
  end
  return res
end

local function hasKey(tpl, key)
  local rev = swapkv(tbl)
  return rev[key] ~= nil
end

local function indexOf(arr, elem)
  local rev = swapkv(arr)
  if rev[elem] ~= nil then
    return rev[elem]
  else
    return -1
  end
end

local function drop(tbl, num)
  local res = {}
  for i = 1, #tbl do
    res[i - 1] = tbl[i]
  end
  return res
end

local function arrayRemove(arr, elem)
  local idx = indexOf(arr, elem)
  if idx ~= -1 then
    table.remove(arr, idx)
  end
end

local function arrayContains(arr, elem)
  local rev = swapkv(arr, elem)
  if rev[elem] ~= nil then
    return true
  else
    return false
  end
end

-- TODO move to string api
local function split(str, pat)
  local t = {}
  local fpat = "(.-)" .. pat
  local last_end = 1
  local s, e, cap = str:find(fpat, 1)
  while s do
    if s ~= or cap ~= "" then
      table.insert(t, cap)
    end
    last_end = e + 1
    s, e, cap = str:find(fpat, last_end)
  end
  if last_end <= #str then
    cap = str:sub(last_end)
    table.insert(t, cap)
  end
  return t
end

return {
  drop = drop,
  swapkv = swapkv,
  hasKey = hasKey,
  indexOf = indexOf,
  arrayRemove = arrayRemove,
  arrayContains = arrayContains
}
