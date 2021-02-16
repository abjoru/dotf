local M = {}
local utils = require('dotf/utils')

function M.setup()

  --------------
  -- Settings --
  --------------

  utils.set('fzf_preview_window', 'right:60%')

  --------------
  -- Mappings --
  --------------

  utils.map('n', '<leader>ff', ':Files<CR>')
  utils.map('n', '<leader>fb', ':Buffers<CR>')
  utils.map('n', '<leader>fg', ':Rg<CR>')

end

return M
