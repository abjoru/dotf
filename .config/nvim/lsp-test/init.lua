local cmd = vim.cmd
local dotf = require('dotf')

-- Global settings
--require('dotf/settings').setup()
dotf.before()

-------------
-- PLUGINS --
-------------

cmd [[packadd packer.nvim]]
require('packer').startup(function(use)
  use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install', cmd = 'MarkdownPreview'}
  use {'savq/paq-nvim', opt = true}
  use {'sheerun/vim-polyglot'}
  use {'wbthomason/packer.nvim', opt = true}
  use {'windwp/nvim-autopairs'}
  use {'Yggdroot/indentLine'}

  -- Git
  use {'airblade/vim-gitgutter'}
  use {'tpope/vim-fugitive'}
  use {'junegunn/gv.vim'}

  -- LSP & Lang
  use {'neovim/nvim-lspconfig'}
  use {'nvim-treesitter/nvim-treesitter'}
  use {'nvim-lua/completion-nvim', requires = {{'hrsh7th/vim-vsnip'}, {'hrsh7th/vim-vsnip-integ'}}}
  use {'scalameta/nvim-metals'}
  use {'andrejlevkovitch/vim-lua-format'}

  -- Search
  use {'junegunn/fzf'}
  use {'junegunn/fzf.vim'}
  --use {'ojroques/nvim-lspfuzzy'}

  -- UI
  use {'kyazdani42/nvim-web-devicons'}
  use {'kyazdani42/nvim-tree.lua'}
  use {'romgrk/barbar.nvim'}
  use {'glepnir/galaxyline.nvim'}
  --use {'morhetz/gruvbox'}
  use {'npxbr/gruvbox.nvim', requires = {'tjdevries/colorbuddy.vim'}}
  use {'liuchengxu/vim-which-key'}
  use {'norcalli/nvim-colorizer.lua'}
  use {'mhinz/vim-startify'}
  use {'caenrique/nvim-toggle-terminal'}
end)

-- Configure Plugins
--require('dotf/ui/settings').setup()
--require('dotf/search/settings').setup()
--require('dotf/lsp/settings').setup()
dotf.configure_plugins()

-- FIXME move these to plugin settings
--require('dotf/ui/statusline').setup()
--require('nvim-autopairs').setup()
dotf.after()
