local cmd = vim.cmd
local dotf = require('dotf')
local exec = vim.api.nvim_command
local fn = vim.fn

--------------------
-- Install Packer --
--------------------

local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  exec('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  exec('packadd packer.nvim')
end

dotf.before()

-------------
-- PLUGINS --
-------------

cmd [[packadd packer.nvim]]
require('packer').startup(function(use)
  -- Package Managers
  use { "wbthomason/packer.nvim", opt = true }
  --use {'savq/paq-nvim', opt = true}

  -- Git
  use { "tpope/vim-fugitive" }
  --use {'airblade/vim-gitgutter'}
  --use {'junegunn/gv.vim'}

  -- LSP & Lang
  use { "glepnir/lspsaga.nvim" }
  use { "hrsh7th/nvim-compe", requires = { { "hrsh7th/vim-vsnip" } } }
  use { "mfussenegger/nvim-dap" }
  use { "neovim/nvim-lspconfig" }
  use { "nvim-treesitter/nvim-treesitter" }
  use { "nvim-treesitter/playground" }
  use { "scalameta/nvim-metals" }
  use { "ckipp01/scala-utils.nvim", requires = { "nvim-lua/plenary.nvim" } }
  use { "kevinhwang91/nvim-bqf" }
  use { "sheerun/vim-polyglot" }
  --use {'nvim-lua/completion-nvim', requires = {
    --{'hrsh7th/vim-vsnip'}, 
    --{'hrsh7th/vim-vsnip-integ'},
    --{'steelsojka/completion-buffers'}
  --}}
  --use {'andrejlevkovitch/vim-lua-format'}
  --use {'preservim/nerdcommenter'}

  -- Search
  --use {'junegunn/fzf'}
  --use {'junegunn/fzf.vim'}

  -- UI
  use { "glepnir/galaxyline.nvim" }
  use { "joshdick/onedark.vim" }
  use { "kyazdani42/nvim-web-devicons" }
  use { "norcalli/nvim-colorizer.lua" }
  use { "junegunn/goyo.vim", opt = true }
  use { "liuchengxu/vista.vim" }
  use { "machakann/vim-sandwich" }
  use { "tpope/vim-vinegar" }
  use { "windwp/nvim-autopairs" }
  use { "Yggdroot/indentLine" }
  use {
    "nvim-telescope/telescope.nvim",
    requires = {
      { "nvim-lua/popup.nvim" },
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-fzy-native.nvim" },
    },
  }
  use {'kyazdani42/nvim-tree.lua'}
  use {'romgrk/barbar.nvim'}
  --use {'npxbr/gruvbox.nvim', requires = {'rktjmp/lush.nvim'}}
  --use {'liuchengxu/vim-which-key'}
  --use {'norcalli/nvim-colorizer.lua'}
  use {'glepnir/dashboard-nvim'}
  use {'caenrique/nvim-toggle-terminal'}
  --use {'schickling/vim-bufonly'}
  --use {'mariappan/dragvisuals.vim'}

  -- Other
  --use {'tpope/vim-scriptease'}


  -- Grabbed from ckipp
  --use({ "wakatime/vim-wakatime" })
  --use({ "wlangstroth/vim-racket" })

end)

dotf.configure_plugins()
dotf.after()
