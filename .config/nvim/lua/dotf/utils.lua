local M = {}
local api = vim.api

-- Create a new mapping for a given mode.
function M.map(mode, lhs, rhs, opts)
  local options = {noremap = true, silent = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Set an option on the given scope.
function M.opt(scope, key, value)
  local scopes = {o = vim.o, b = vim.bo, w = vim.wo}
  scopes[scope][key] = value
  if scope ~= 'o' then
    scopes['o'][key] = value
  end
end

-- Set a variable.
function M.set(key, value)
  api.nvim_set_var(key, value)
end

return M
