local M = {}

function M.setup(lsp)
  lsp.hls.setup {
    settings = {
      haskell = {
        formattingProvider = 'stylish-haskell'
      }
    }
  }
end

return M
