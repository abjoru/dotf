local M = {}

function M.before()
  require('dotf/settings').setup()
end

function M.configure_plugins()
  require('dotf/ui/settings').setup()
  require('dotf/search/settings').setup()
  require('dotf/lsp/settings').setup()
  require('nvim-autopairs').setup()
end

function M.after()
  require('dotf/ui/statusline').setup()
end

return M
