local M = {}
local fn = vim.fn
local cmd = vim.cmd
local utils = require('dotf/utils')
local lsp_config = require('lspconfig')
local metals = require('metals')
local completion = require('completion')

function M.setup()

  --------------
  -- Settings --
  --------------

  --------------
  -- Mappings --
  --------------

  -- LSP
  utils.map('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>')
  utils.map('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
  utils.map('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
  utils.map('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')
  utils.map('n', 'gs', '<cmd>lua vim.lsp.buf.document_symbol()<CR>')
  utils.map('n', 'gw', '<cmd>lua vim.lsp.buf.workspace_symbol()<CR>')

  -- TODO evaluate these and remap if neccessary
  utils.map('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
  --utils.map('n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', {nowait = true})
  utils.map('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>')
  utils.map('n', '<leader>ws', '<cmd>lua require"metals".worksheet_hover()<CR>')
  utils.map('n', '<leader>a', '<cmd>lua require"metals".open_all_diagnostics()<CR>')
  utils.map('n', '<leader>d', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>') -- buffer diagnostics only
  utils.map('n', '[c', '<cmd>lua vim.lsp.diagnostic.goto_prev { wrap = false }<CR>')
  utils.map('n', ']c', '<cmd>lua vim.lsp.diagnostic.goto_next { wrap = false }<CR>')

  -- Completion
  utils.map('i', '<S-Tab>', 'pumvisible() ? "\\<C-p>" : "\\<Tab>"', {expr = true})      -- shift-tab nav prev
  utils.map('i', '<Tab>', 'pumvisible() ? "\\<C-n>" : "\\<Tab>"', {expr = true})        -- tab nav next
  utils.map('i', '<CR>', 'pumvisible() ? "\\<C-y>" : "\\<C-g>u\\<CR>"', {expr = true})  -- select with enter

  -- Comments
  utils.map('n', '<leader>cc', ':NERDComment(1, "invert")')

  --------------
  -- Commands --
  --------------

  cmd [[autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o]]
  cmd [[autocmd FileType markdown setlocal textwidth=80]]
  cmd [[autocmd BufEnter *.js call matchadd('ColorColumn', '\%81v', 100)]]
  cmd [[autocmd BufReadPost,BufNewFile *.md,*.txt,COMMIT_EDITMSG set wrap linebreak nolist spell spelllang=en_us complete+=kspell]]
  cmd [[autocmd BufReadPost,BufNewFile .html,*.txt,*.md,*.adoc set spell spelllang=en_us]]

  -- LSP
  cmd [[augroup lsp]]
  cmd [[autocmd!]]
  cmd [[autocmd FileType scala setlocal omnifunc=v:lua.vim.lsp.omnifunc]]
  cmd [[autocmd FileType scala,sbt lua require("metals").initialize_or_attach(metals_config)]]
  cmd [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
  cmd [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
  cmd [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
  cmd [[augroup end]]

  cmd [[hi! link LspReferenceText CursorColumn]]
  cmd [[hi! link LspReferenceRead CursorColumn]]
  cmd [[hi! link LspReferenceWrite CursorColumn]]

  ----------------
  -- LSP Config --
  ----------------

  fn.sign_define('LspDiagnosticsSignError', {text = '✘', texthl = 'LspDiagnosticsDefaultError'})
  fn.sign_define('LspDiagnosticsSignWarning', {text = '', texthl = 'LspDiagnosticsDefaultWarning'})

  local shared_diagnostic_settings = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {virtual_text = {prefix = '', truncated = true}}
  )

  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  lsp_config.util.default_config = vim.tbl_extend('force', lsp_config.util.default_config, {
    handlers = {['textDocument/publishDiagnostics'] = shared_diagnostic_settings},
    on_attach = completion.on_attach,
    capabilities = capabilities
  })

  -- Metals
  metals_config = metals.bare_config
  metals_config.settings = {
    showImplicitArguments = true,
    excludedPackages = {'akka.actor.typed.javadsl', 'com.github.swagger.akka.javadsl'}
  }

  metals_config.on_attach = function() completion.on_attach(); end
  metals_config.init_options.statusBarProvider = 'on'
  metals_config.handlers['textDocument/publishDiagnostics'] = shared_diagnostic_settings
  metals_config.capabilities = capabilities

  -- Others
  lsp_config.hls.setup {}
  lsp_config.dockerls.setup {}
  lsp_config.html.setup {}
  lsp_config.jsonls.setup {
    commands = {
      Format = {
        function()
          vim.lsp.buf.range_formatting({}, {0, 0}, {fn.line('$'), 0})
        end
      }
    }
  }
  lsp_config.tsserver.setup {}
  lsp_config.yamlls.setup {}
  lsp_config.racket_langserver.setup {}

  ----------------------
  -- Treesitter Setup --
  ----------------------

  require('nvim-treesitter.configs').setup {
    ensure_installed = {'html', 'javascript', 'yaml', 'css', 'toml', 'lua', 'json'},
    highlight = {enable = true}
  }
end

return M
