return require('packer').startup(function(use)
  use 'neovim/nvim-lspconfig'
  use 'wellle/tmux-complete' --source for tmux panes
  use 'brooth/far.vim' --asynchronous search and replace operations on a set of files
  use 'Shougo/echodoc.vim' --print completed documention
  use 'Shougo/neoinclude.vim' --complete candidates from included files and path
  use 'Shougo/context_filetype.vim' --add context filype feature
  use 'tpope/vim-surround' --useging that allow to surround text
  use 'tpope/vim-fugitive' --Git wrapper for vim
  use {'junegunn/fzf',run = 'cd ~/.fzf && ./install -all' }
  use 'junegunn/fzf.vim'
  use 'windwp/nvim-autopairs'
  use 'wincent/loupe'
  use 'vim-airline/vim-airline'
  use 'edkolev/tmuxline.vim'
  --use 'morhetz/gruvbox'
  use 'npxbr/gruvbox.nvim'
  use 'rktjmp/lush.nvim'
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}  -- We recommend updating the parsers on update
  --use 'nvim-lua/completion-nvim'
  use 'udalov/kotlin-vim'
  use 'hrsh7th/nvim-compe'
  --use 'norcalli/snippets.nvim'
  use 'rafamadriz/friendly-snippets'
  use 'kyazdani42/nvim-web-devicons' -- for file icons
  --use 'nvim-nonicons'
  use 'kyazdani42/nvim-tree.lua'
  use 'hrsh7th/vim-vsnip-integ'
  use 'hrsh7th/vim-vsnip'
end)
