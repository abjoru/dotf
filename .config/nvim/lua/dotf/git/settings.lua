local M = {}
local g = vim.g
local map = require('dotf/utils').map

function M.setup()
  --------------
  -- Settings --
  --------------

  g.gitgutter_map_keys = 0

  --------------
  -- Mappings --
  --------------

  map('n', '<leader>gc', ':Gcommit<CR>')
  map('n', '<leader>gs', ':Gstatus<CR>')
  map('n', '<leader>gl', ':Commits!<CR>')
  map('n', '<leader>gL', ':BCommits!<CR>')
  map('n', '<leader>gd', ':Gdiff<CR>')
end

return M
