local M = {}
local g = vim.g
local utils = require('dotf/utils')

function M.setup()
  --------------
  -- Settings --
  --------------

  g.gitgutter_map_keys = 0

  --------------
  -- Mappings --
  --------------

  utils.map('n', '<leader>gc', ':Gcommit<CR>')
  utils.map('n', '<leader>gs', ':Gstatus<CR>')
  utils.map('n', '<leader>gl', ':Commits!<CR>')
  utils.map('n', '<leader>gL', ':BCommits!<CR>')
  utils.map('n', '<leader>gd', ':Gdiff<CR>')
end

return M
