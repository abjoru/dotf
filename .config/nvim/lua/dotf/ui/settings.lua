local M = {}
local fn = vim.fn
local cmd = vim.cmd
local utils = require('dotf/utils')
--local palette = require('gruvbox.palette')

local haskellLogo = {
  '',
  '     ############(    **///////*//*                                           ',
  '     ,############.   .///*///*////.                                          ',
  '       (###########(    //////*//////                                         ',
  '        ,############.   .***********/                                        ',
  '          ############(    ///*///////**                                      ',
  '           *############    ,/*///*///*//                                     ',
  '             ############(    ////////*///*    (###########################   ',
  '              *############    .*/*/*/*/*/*/.   .##########################   ',
  '                ############(    *////*///////    #########################   ',
  '                 ,############.   .///*///*////.   ,#######################   ',
  '                   ############(    //*///////*/*                             ',
  '                   ############/    /************/                            ',
  '                 /############    ,///*///////*////*    (##################   ',
  '                ############/    /*///*///*///*///*//    ,#################   ',
  '              /############    *//////*///////*///////*    (###############   ',
  '             ############/    //*/*/*/*/*/,.*/*/*/*/*/*/.   .##############   ',
  '           /############    ,/*///////*//    /*///////*///                    ',
  '          ############(    ///*///*///**      .///*///*///*.                  ',
  '        /############    ,////*///////          //////*/////*                 ',
  '       ############/    *************            ./***********                ',
  '     /############    ,///////*////                ///*///////**              ',
  '    ############/    /*///*///*//,                  ./*///*///*//.            ',
  ''
}

function M.setup()
  print('ui/settings called...')
  
  ---------------------
  -- Colors & Themes --
  ---------------------

  --require('colorbuddy').colorscheme('gruvbox')
  vim.o.background = "dark"
  vim.cmd([[colorscheme gruvbox]])

  --require('dotf/ui/devicons').setup {
    --override = {
      --diropen = {
        --icon = '',
        --color = palette.bright_yellow,
        --name = "DirOpen"
      --};
      --dirclosed = {
        --icon = '',
        --color = palette.bright_yellow,
        --name = "DirClosed"
      --};
      --['dirsymlink'] = {
        --icon = '',
        --color = palette.bright_yellow,
        --name = "DirSymlink"
      --}
    --}
  --}

  --cmd('hi NERDTreeOpenable guifg=' .. palette.light3)
  --cmd('hi NERDTreeClosable guifg=' .. palette.light4)
  --cmd('hi NERDTreeDir guifg=' .. palette.bright_orange)

  --------------
  -- Settings --
  --------------

  -- Startpage
  utils.set('dashboard_custom_header', haskellLogo)
  utils.set('dashboard_default_executive', 'fzf')
  utils.set('indentLine_fileTypeExclude', {'dashboard'})
  utils.set('dashboard_custom_shortcut', {
      ['last_session'] = 'SPC l s',
      ['find_history'] = 'SPC f h',
      ['find_file']    = 'SPC f f',
      ['new_file']     = 'SPC n f',
      ['change_colorscheme'] = 'SPC c t',
      ['find_word']          = 'SPC f w',
      ['book_marks']         = 'SPC f b'
    })

  -- Terminal
  utils.set('auto_start_insert', 0)
  utils.set('open_in_insert_mode', 1)

  -- Tree
  --utils.set('NERDTreeWinSize', 60)
  --utils.set('NERDTreeRespectWildIgnore', 1)
  utils.set('nvim_tree_auto_open', 1)
  utils.set('nvim_tree_auto_close', 1)
  utils.set('nvim_tree_width', 60)
  --utils.set('nvim_tree_ignore', ['.git'])
  utils.set('nvim_tree_git_hl', 1)
  utils.set('nvim_tree_hide_dotfiles', 1)
  utils.set('nvim_tree_indent_markers', 1)
  utils.set('nvim_tree_group_empty', 1)
  --utils.set('nvim_tree_icons', {
    --'default': '',
    --'symlink': '',
    --'git': {
      --'unstaged': "✗",
      --'staged': "✓",
      --'unmerged': "",
      --'renamed': "➜",
      --'untracked': "★"
    --},
    --'folder': {
      --'default': "",
      --'open': "",
      --'empty': "",
      --'empty_open': "",
      --'symlink': "",
      --'symlink_open': "",
    --}
  --})

  --------------
  -- Mappings --
  --------------

  -- move more sensibly when line wrapping enabled
  cmd 'nmap k gk'
  cmd 'nmap j gj'

  -- switch windows with CTRL+hjkl
  utils.map('n', '<C-J>', '<C-W><C-J>')
  utils.map('n', '<C-K>', '<C-W><C-K>')
  utils.map('n', '<C-L>', '<C-W><C-L>')
  utils.map('n', '<C-H>', '<C-W><C-H>')

  -- move blocks of code
  utils.map('v', '<', '<gv')
  utils.map('v', '>', '>gv')

  cmd 'vmap <expr> <UP> DVB_Drag("up")'
  cmd 'vmap <expr> <DOWN> DVB_Drag("down")'

  -- clear search highlight
  utils.map('n', '<leader><leader>', ':noh<CR>')

  -- Yank clipboard management
  utils.map('v', '<localleader>y', '"+y')
  utils.map('n', '<localleader>y', '"+y')
  utils.map('n', '<localleader>yy', '"+yg_')

  -- Paste clipboard management
  utils.map('v', '<localleader>p', '"+p')
  utils.map('n', '<localleader>p', '"+p')
  utils.map('v', '<localleader>pp', '"+p')
  utils.map('n', '<localleader>pp', '"+p')

  -- Tree
  utils.map('', '<F2>', ':NvimTreeToggle<CR>')
  utils.map('n', '<leader>fr', ':NvimTreeFindFile<CR>')

  -- Tabbar
  utils.map('', '<localleader>[', ':BufferPrevious<CR>')
  utils.map('', '<localleader>]', ':BufferNext<CR>')
  utils.map('n', '<leader>bs', ':BufferPick<CR>')
  utils.map('n', '<leader>bd', ':bd<CR>')
  utils.map('n', '<leader>bo', ':BufOnly<CR>')

  -- Leader guide
  utils.map('', '<leader>', ':WhichKey \'<Space>\'<CR>')
  utils.map('', '?', ':WhichKey \'\'<CR>')

  -- Terminal
  utils.map('', '<F3>', ':ToggleTerminal<CR>')
  utils.map('t', '<C-h>', '<C-\\><C-N><C-w>h')
  utils.map('t', '<C-j>', '<C-\\><C-N><C-w>j')
  utils.map('t', '<C-k>', '<C-\\><C-N><C-w>k')
  utils.map('t', '<C-l>', '<C-\\><C-N><C-w>l')

  --------------
  -- Commands --
  --------------

  -- Comment boxes
  cmd [[autocmd FileType vim vnoremap <buffer> ,cc !boxes -f ~/.config/nvim/cfg/boxes -d vim-box<CR>]]
  cmd [[autocmd FileType scala vnoremap <buffer> ,cc !boxes -f ~/.config/nvim/cfg/boxes -d scala<CR>]]

  -- Fix startify
  cmd [[augroup fixStartify]]
  cmd [[au User Startified nmap <buffer> k k]]
  cmd [[au User Startified nmap <buffer> j j]]
  cmd [[au User Startified nmap <buffer> q :q<CR>]]
  cmd [[augroup END]]

  -- Terminal stuff
  cmd [[command! -nargs=0 -bar DfTerminalOpen rightbelow split | terminal]]
  cmd [[command! -nargs=0 -bar DfTerminalCmd call feedkeys(":! ")]]

  cmd [[augroup terminalConfig]]
  cmd [[au!]]
  cmd [[au TermOpen * setlocal statusline=%{b:term_title}]]
  cmd [[au BufEnter * if &buftype == 'terminal' | setlocal statusline=%{b:term_title} | endif]]
  cmd [[augroup END]]

  -- Tree colors
  -- Ref: https://jonasjacek.github.io/colors/
  cmd [[highlight NvimTreeFolderIcon guifg=darkorange3]]
  cmd [[highlight NvimTreeGitDirty guifg=red3]]
  cmd [[highlight NvimTreeIndentMarker guifg=darkorange3]]
  --cmd [[autocmd VimEnter * NERDTree | wincmd p]]

end

return M
