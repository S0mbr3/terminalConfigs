return require('packer').startup(function(use)
  use 'neovim/nvim-lspconfig'
  use 'tami5/lspsaga.nvim'
  -- use 'glepnir/lspsaga.nvim'
  use 'brooth/far.vim' --asynchronous search and replace operations on a set of files
  use 'Shougo/echodoc.vim' --print completed documention
  use 'Shougo/neoinclude.vim' --complete candidates from included files and path
  use 'Shougo/context_filetype.vim' --add context filype feature
  use 'tpope/vim-surround' --useging that allow to surround text
  use 'tpope/vim-fugitive' --Git wrapper for vim
  if vim.fn.has('unix') then
    use {'junegunn/fzf',run = 'cd ~/.fzf && ./install -all' }
    use 'junegunn/fzf.vim'
    use 'edkolev/tmuxline.vim'
    --use 'wellle/tmux-complete' --source for tmux panes
  end
  use 'windwp/nvim-autopairs'
  use 'wincent/loupe'
  --use 'vim-airline/vim-airline'
  use {
  'nvim-lualine/lualine.nvim',
  requires = {'kyazdani42/nvim-web-devicons', opt = true}
}
  --use 'morhetz/gruvbox'
  use 'owozsh/amora'
  use {'ellisonleao/gruvbox.nvim', requires = {'rktjmp/lush.nvim'}}
  use 'nvim-lua/popup.nvim'
  use {'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim'}}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}
  use {'nvim-telescope/telescope-project.nvim'}
  use { "nvim-telescope/telescope-file-browser.nvim" }
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}  -- We recommend updating the parsers on update
  --use 'nvim-lua/completion-nvim'
  use 'udalov/kotlin-vim'
  --use 'norcalli/snippets.nvim'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  use 'rafamadriz/friendly-snippets'
  --use 'nvim-nonicons'
  use {'kyazdani42/nvim-tree.lua', requires = {'kyazdani42/nvim-web-devicons'}} -- for file icons

  use 'hrsh7th/vim-vsnip-integ'
  use 'hrsh7th/cmp-vsnip'
  use 'hrsh7th/vim-vsnip'
  use 'hrsh7th/nvim-cmp'
  use 'github/copilot.vim'
  use 'marko-cerovac/material.nvim'
  use {'kdheepak/lazygit.nvim'}
  use {'lambdalisue/suda.vim'}
  use {
    's1n7ax/nvim-terminal',
    config = function()
        vim.o.hidden = true
        require('nvim-terminal').setup()
    end,
}
  use {'edluffy/hologram.nvim'} -- to see images inside neovim
  use {
  'phaazon/hop.nvim',
  branch = 'v1', -- optional but strongly recommended
  config = function()
    -- you can configure Hop the way you like here; see :h hop-config
    require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
  end
}
end)
