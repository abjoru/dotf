local M = {}
local g = vim.g
local fn = vim.fn
local cmd = vim.cmd
local map = require('dotf/utils').map
local lsp_config = require('lspconfig')

function M.setup()

  -------------
  -- Plugins --
  -------------

  require('nvim-autopairs').setup()
  require('nvim-treesitter.configs').setup({
    playground = { enable = true },
    query_linter = {
      enable = true,
      use_virtual_text = true,
      lint_events = { "BufWrite", "CursorHold" },
    },
    ensure_installed = "maintained",
    highlight = { enable = true },
  })
  require('lspsaga').init_lsp_saga({
    server_filetype_map = { metals = { "sbt", "scala" }},
    code_action_prompt = { virtual_text = false },
  })

  --------------
  -- Settings --
  --------------

  g["vim_markdown_conceal"] = 0
  g["vim_markdown_conceal_code_blocks"] = 0
  g["metals_server_version"] = "0.10.3+23-3512dbc6-SNAPSHOT"

  --utils.set('completion_enable_snippet', 'vim-vsnip')
  --utils.set('completion_matching_strategy_list', {'exact', 'substring', 'fuzzy'})
  --utils.set('completion_matching_smart_case', 1)
  --utils.set('completion_trigger_on_delete', 1)
  --utils.set('completion_enable_auto_popup', 1)
  --utils.set('completion_auto_change_source', 1)
  --utils.set('completion_chain_complete_list', {
      --{['complete_items'] = {'lsp'}},
      --{['complete_items'] = {'snippet'}},
      --{['complete_items'] = {'buffers'}},
      --{mode = '<c-p>'},
      --{mode = '<c-n>'}
    --})

  --utils.set('asmsyntax', 'nasm')

  --------------
  -- Mappings --
  --------------

  -- telescope
  map("n", "<leader>ff", [[<cmd>lua require("telescope.builtin").find_files()<CR>]])
  map("n", "<leader>fg", [[<cmd>lua require("telescope.builtin").live_grep()<CR>]])
  map("n", "<leader>fb", [[<cmd>lua require("telescope.builtin").buffers()<CR>]])
  map("n", "<leader>fd", [[<cmd>lua require("telescope.builtin").file_browser()<CR>]])

  -- LSP
  map('n', "K", [[<cmd>lua require("lspsaga.hover").render_hover_doc()<CR>]])
  map('n', 'gd', [[<cmd>lua vim.lsp.buf.definition()<CR>]])
  map('n', 'gi', [[<cmd>lua vim.lsp.buf.implementation()<CR>]])
  map('n', 'gr', [[<cmd>lua vim.lsp.buf.references()<CR>]])
  map('n', "gs", [[<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>]])
  map('n', "gw", [[<cmd>lua lsp_workspace_symbols()<CR>]])

  map('n', "<leader>rn", [[<cmd>lua require("lspsaga.rename").rename()<CR>]])
  map('n', "<leader>ca", [[<cmd>lua require("lspsaga.codeaction").code_action()<CR>]])
  map('v', "<leader>ca", [[<cmd>lua require("lspsaga.codeaction").range_code_action()<CR>]])
  map('n', "<leader>ws", [[<cmd>lua require("metals").worksheet_hover()<CR>]])
  map('n', '<localleader>f', [[<cmd>lua vim.lsp.buf.formatting()<CR>]], {nowait = true})

  map("n", "<leader>tt", [[<cmd>lua require("metals.tvp").toggle_tree_view()<CR>]])
  map("n", "<leader>td", [[<cmd>lua require("metals.tvp").debug_tree()<CR>]])
  map("n", "<leader>tr", [[<cmd>lua require("metals.tvp").reveal_in_tree()<CR>]])

  map('n', '<leader>a', [[<cmd>lua require("metals").open_all_diagnostics()<CR>]])
  map('n', '<leader>d', [[<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>]]) -- buffer diagnostics only
  map("n", "]c", [[<cmd>lua require("lspsaga.diagnostic").lsp_jump_diagnostic_next()<CR>]])
  map("n", "[c", [[<cmd>lua require("lspsaga.diagnostic").lsp_jump_diagnostic_prev()<CR>]])
  map("n", "<leader>ln", [[<cmd>lua vim.lsp.diagnostic.get_line_diagnostics()<CR>]])

  -- Completion
  map('i', '<S-Tab>', 'pumvisible() ? "\\<C-p>" : "\\<Tab>"', {expr = true})      -- shift-tab nav prev
  map('i', '<Tab>', 'pumvisible() ? "\\<C-n>" : "\\<Tab>"', {expr = true})        -- tab nav next fg
  map('i', '<CR>', 'pumvisible() ? "\\<C-y>" : "\\<C-g>u\\<CR>"', {expr = true})  -- select with enter
  --cmd [[imap <silent> <c-p> <Plug>(completion_trigger)]]

  -- nvim-dap
  map("n", "<leader>dc", [[<cmd>lua require("dap").continue()<CR>]])
  map("n", "<leader>dr", [[<cmd>lua require("dap").repl.toggle()<CR>]])
  map("n", "<leader>ds", [[<cmd>lua require("dap.ui.variables").scopes()<CR>]])
  map("n", "<leader>dtb", [[<cmd>lua require("dap").toggle_breakpoint()<CR>]])
  map("n", "<leader>dso", [[<cmd>lua require("dap").step_over()<CR>]])
  map("n", "<leader>dsi", [[<cmd>lua require("dap").step_into()<CR>]])

  -- Comments
  map('n', '<leader>cc', ':NERDComment(1, "invert")')

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
  --cmd [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
  --cmd [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
  --cmd [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
  cmd [[augroup end]]

  --cmd [[autocmd BufEnter * lua require'completion'.on_attach()]]

  cmd [[hi! link LspReferenceText CursorColumn]]
  cmd [[hi! link LspReferenceRead CursorColumn]]
  cmd [[hi! link LspReferenceWrite CursorColumn]]

  cmd([[hi! link LspSagaFinderSelection CursorColumn]])
  cmd([[hi! link LspSagaDocTruncateLine LspSagaHoverBorder]])

  cmd([[command! Format lua vim.lsp.buf.formatting()]])

  ----------------
  -- LSP Config --
  ----------------

  fn.sign_define('LspDiagnosticsSignError', {text = '✘', texthl = 'LspDiagnosticsDefaultError'})
  fn.sign_define('LspDiagnosticsSignWarning', {text = '', texthl = 'LspDiagnosticsDefaultWarning'})
  fn.sign_define("LspDiagnosticsSignInformation", { text = "▬" })
  fn.sign_define("LspDiagnosticsSignHint", { text = "▬" })

  local shared_diagnostic_settings = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { virtual_text = false })
  --local lsp_config = require("lspconfig")
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true

  lsp_config.util.default_config = vim.tbl_extend("force", lsp_config.util.default_config, {
    handlers = {
      ["textDocument/publishDiagnostics"] = shared_diagnostic_settings,
    },
    capabilities = capabilities,
  })

  metals_config = require("metals").bare_config
  local dap = require("dap")

  require('dotf.lsp.scala').setup(metals_config, capabilities, dap, shared_diagnostic_settings)
  require('dotf.lsp.sumneko').setup(lsp_config)
  require('dotf.lsp.haskell').setup(lsp_config)

  -- Others
  --local servers = {'hls', 'vimls'}
  --for _, lsp in ipairs(servers) do
    --lsp_config[lsp].setup {
      --on_attach = onAttach
    --}
  --end

  -- TODO cleanup rest...
  
  --local onAttach = function(client, bufnr)
    --completion.on_attach()

    -- format on save for selected buffers
    --vim.api.nvim_command[[autocmd BufWritePre *.scala lua vim.lsp.buf.formatting_sync(nil, 1000)]]
    --vim.api.nvim_command[[autocmd BufWritePre *.scala lua require('metals').organize_imports()]]
    --vim.api.nvim_command[[autocmd BufWritePre *.sbt lua vim.lsp.buf.formatting_sync(nil, 1000)]]
    --vim.api.nvim_command[[autocmd BufWritePre *.hs lua vim.lsp.buf.formatting_sync(nil, 1000)]]
  --end

  --lsp_config.util.default_config = vim.tbl_extend('force', lsp_config.util.default_config, {
    --handlers = {['textDocument/publishDiagnostics'] = shared_diagnostic_settings},
    --on_attach = onAttach,
    --capabilities = capabilities
  --})



  --lsp_config.hls.setup {
    --settings = {
      --haskell = {
        --formattingProvider = 'stylish-haskell'
      --}
    --}
  --}

  --lsp_config.sumneko_lua.setup {
    --cmd = {sumneko_binary, "-E", sumneko_root_path .. '/main.lua'};
    --settings = {
      --Lua = {
        --runtime = {
          --version = 'LuaJIT',
          --path = vim.split(package.path, ';')
        --},
        --diagnostics = {
          --globals = {'vim'}
        --},
        --workspace = {
          --library = {
            --[fn.expand('$VIMRUNTIME/lua')] = true,
            --[fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true
          --}
        --}
      --}
    --}
  --}

  --lsp_config.dockerls.setup {}
  --lsp_config.html.setup {}
  --lsp_config.jsonls.setup {
    --commands = {
      --Format = {
        --function()
          --vim.lsp.buf.range_formatting({}, {0, 0}, {fn.line('$'), 0})
        --end
      --}
    --}
  --}
  --lsp_config.tsserver.setup {}
  --lsp_config.yamlls.setup {}
  --lsp_config.racket_langserver.setup {}

  ----------------------
  -- Treesitter Setup --
  ----------------------

  --require('nvim-treesitter.configs').setup {
    --ensure_installed = {'html', 'javascript', 'yaml', 'css', 'toml', 'lua', 'json'},
    --highlight = {enable = true}
  --}

end

local function lsp_workspace_symbols()
  local input = fn.input("Query: ")
  vim.api.nvim_command("normal :esc<CR>")
  if not input or #input == 0 then
    return
  end
  require('telescope.builtin').lsp_workspace_symbols({ query = input })
end

return M
