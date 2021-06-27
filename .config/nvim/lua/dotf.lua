local M = {}

-- Check if the installer flag has been set.
-- We do not want to load plugin configs for
-- plugins that have yet to be installed!
local function is_install()
  if not vim.g.dotf_run_install then
    return false
  else
    return vim.g.dotf_run_install == 1
  end
end

function M.before()
  require('dotf/settings').setup()
end

function M.configure_plugins()
  if not is_install() then
    require('dotf/ui/settings').setup()
    require('dotf/git/settings').setup()
    require('dotf/lsp/settings').setup()
  end
end

function M.after()
  if not is_install() then
    require('dotf/ui/statusline').setup()
  end
end

return M
