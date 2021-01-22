local M = {}
local fn = vim.fn

local function pushToCurrentBranch()
  -- execute ':Gwrite'
  local branch = fn['fugitive#statusline']()

  -- execute ':Git push origin ' . branch
end

function M.setup()
end

return M
