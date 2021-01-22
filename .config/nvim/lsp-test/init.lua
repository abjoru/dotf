local cmd = vim.cmd
local dotf = require('dotf')

dotf.before()

-------------
-- PLUGINS --
-------------

cmd [[packadd packer.nvim]]
require('packer').startup(function(use)
  -- Package Managers
  use {'savq/paq-nvim', opt = true}
  use {'wbthomason/packer.nvim', opt = true}

  -- Git
  use {'airblade/vim-gitgutter'}
  use {'tpope/vim-fugitive'}
  use {'junegunn/gv.vim'}

  -- LSP & Lang
  use {'sheerun/vim-polyglot'}
  use {'neovim/nvim-lspconfig'}
  use {'nvim-treesitter/nvim-treesitter'}
  use {'nvim-lua/completion-nvim', requires = {{'hrsh7th/vim-vsnip'}, {'hrsh7th/vim-vsnip-integ'}}}
  use {'scalameta/nvim-metals'}
  use {'andrejlevkovitch/vim-lua-format'}
  use {'preservim/nerdcommenter'}
  use {'windwp/nvim-autopairs'}
  use {'Yggdroot/indentLine'}
  use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install', cmd = 'MarkdownPreview'}

  -- Search
  use {'junegunn/fzf'}
  use {'junegunn/fzf.vim'}

  -- UI
  use {'kyazdani42/nvim-web-devicons'}
  use {'kyazdani42/nvim-tree.lua'}
  use {'romgrk/barbar.nvim'}
  use {'glepnir/galaxyline.nvim'}
  use {'npxbr/gruvbox.nvim', requires = {'tjdevries/colorbuddy.vim'}}
  use {'liuchengxu/vim-which-key'}
  use {'norcalli/nvim-colorizer.lua'}
  use {'mhinz/vim-startify'}
  use {'caenrique/nvim-toggle-terminal'}
  use {'schickling/vim-bufonly'}
  use {'mariappan/dragvisuals.vim'}
end)

dotf.configure_plugins()
dotf.after()
