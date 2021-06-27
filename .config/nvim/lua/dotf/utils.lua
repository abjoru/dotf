local M = {}
local api = vim.api

function M.toggle_nums()
  vim.wo.number = not vim.wo.number
end

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

function M.set_icon_colors(config, filetypes)
  local joined_filetypes = table.concat(filetypes, ",")
  api.nvim_command('augroup DeviconsColors')
  api.nvim_command('autocmd!')
  for group, icons in pairs(config) do
    api.nvim_command(string.format('autocmd FileType %s syntax match %s /\\v%s./ containedin=ALL', joined_filetypes, group, table.concat(icons, '.|')))
  end
  api.nvim_command('augroup END')
end

return M
