local M = {}
local fn = vim.fn

local function sumneko_root_path()
  return fn.expand('~') .. '/.local/share/lua-lsp'
end

local function sumneko_binary()
  local system_name
  if fn.has("mac") == 1 then
    system_name = "macOS"
  elseif fn.has("unix") == 1 then
    system_name = "Linux"
  else
    print("Unsupported system for sumneko")
  end

  return sumneko_root_path() .. '/bin/' .. system_name .. '/lua-language-server'
end

function M.setup(lsp)
  lsp.sumneko_lua.setup({
    cmd = { sumneko_binary(), "-E", sumneko_root_path() .. '/main.lua'},
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT", -- since using mainly for neovim
          path = vim.split(package.path, ";"),
        },
        diagnostics = { globals = { "vim", "it", "describe" } },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = {
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
          },
        },
        telemetry = { enable = false },
      },
    },
  })
end

return M
