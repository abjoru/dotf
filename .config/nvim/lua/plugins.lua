local M = require('packer').startup(function(use)
  use {'airblade/vim-gitgutter'}
  use {'andrejlevkovitch/vim-lua-format'}

  use {
    'iamcco/markdown-preview.nvim',
    run = 'cd app && yarn install',
    cmd = 'MarkdownPreview'
  }


  use {'neovim/nvim-lspconfig'}
  use {'norcalli/nvim-colorizer.lua'}
  use {
    'nvim-lua/completion-nvim',
    requires = {{'hrsh7th/vim-vsnip'}, {'hrsh7th/vim-vsnip-integ'}}
  }
  use {'nvim-treesitter/nvim-treesitter'}
  use {'savq/paq-nvim', opt = true}
  use {'scalameta/nvim-metals'}
  use {'sheerun/vim-polyglot'}
  use {'tpope/vim-fugitive'}
  use {'wbthomason/packer.nvim', opt = true}
  use {'windwp/nvim-autopairs'}
  use {'Yggdroot/indentLine'}

  -- UI
  use {'kyazdani42/nvim-web-devicons'}
  use {'kyazdani42/nvim-tree.lua'}
  use {'romgrk/barbar.nvim'}
  use {'glepnir/galaxyline.nvim'}
  use {'morhetz/gruvbox'}
  use {'liuchengxu/vim-which-key'}
end)

require('ui/settings')

return M
