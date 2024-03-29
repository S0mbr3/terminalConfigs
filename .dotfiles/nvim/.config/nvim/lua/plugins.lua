return require('packer').startup(function(use)
use({
  "jackMort/ChatGPT.nvim",
    config = function()
      require("chatgpt").setup({
        -- optional configuration
      })
    end,
    requires = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim"
    }
  })
  use {
    "nvim-neorg/neorg",
    run = ":Neorg sync-parsers", -- This is the important bit!
    config = function()
      require("neorg").setup {
        -- configuration here
      }
    end,
  }
  use "folke/neodev.nvim"
  use {"jbyuki/nabla.nvim"}
  use 'neovim/nvim-lspconfig'
  use 'mfussenegger/nvim-dap'
  use 'simrat39/rust-tools.nvim'
  use 'p00f/clangd_extensions.nvim'
  --use 'tami5/lspsaga.nvim'
  use 'williamboman/mason.nvim'                                                        -- Manage external editor tooling i.e LSP servers
  use 'williamboman/mason-lspconfig.nvim'
  --use 'kkharji/lspsaga.nvim'
  use ({
    'glepnir/lspsaga.nvim',
    branch = "main",
    config = function()
      require('lspsaga').setup({})
    end,
    })
  --use 'Shougo/neoinclude.vim' --complete candidates from included files and path
  use 'numToStr/Comment.nvim'
  use 'brooth/far.vim' --asynchronous search and replace operations on a set of files
  use 'Shougo/echodoc.vim' --print completed documention
  use 'Shougo/context_filetype.vim' --add context filype feature
  use 'windwp/nvim-autopairs'
  use 'ThePrimeagen/harpoon'
  use {'lambdalisue/suda.vim'}
  use {
    's1n7ax/nvim-terminal',
    config = function()
        vim.o.hidden = true
        require('nvim-terminal').setup()
    end,
}

  use 'tpope/vim-surround' --useging that allow to surround text
  use 'tpope/vim-fugitive' --Git wrapper for vim
  use 'tpope/vim-rhubarb'                                                              -- Fugitive-companion to interact with github
  use {'kdheepak/lazygit.nvim'}
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }            -- Add git related info in the signs columns and popups
  if vim.fn.has('unix') then
    use {'junegunn/fzf',run = 'cd ~/.fzf && ./install -all' }
    use 'junegunn/fzf.vim'
    use 'edkolev/tmuxline.vim'
    --use 'wellle/tmux-complete' --source for tmux panes
  end
  --use 'wincent/loupe' -- Add very magic to all searchers and replacers
  --use 'vim-airline/vim-airline'
  use {
  'nvim-lualine/lualine.nvim',
  requires = {'kyazdani42/nvim-web-devicons', opt = true}
}
  -- for tmux ressurrect allowing to restore neovim/vim sessions
--  use 'tpope/vim-obsession'

  --use 'morhetz/gruvbox'
  use { 'catppuccin/nvim', as = 'catppuccin'}
  use 'folke/tokyonight.nvim'
  use "tiagovla/tokyodark.nvim"
  use 'marko-cerovac/material.nvim'
  use 'owozsh/amora'
  use {'ellisonleao/gruvbox.nvim', requires = {'rktjmp/lush.nvim'}}
  use 'lukas-reineke/indent-blankline.nvim'                                            -- Add indentation guides even on blank lines
  use 'nvim-lua/popup.nvim'
use {
  'gelguy/wilder.nvim'
}
use {"folke/noice.nvim", requires = {'rcarriga/nvim-notify', 'MunifTanjim/nui.nvim'}}

  use {'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim'}}
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}
  use {'nvim-telescope/telescope-project.nvim'}
  use { "nvim-telescope/telescope-file-browser.nvim" }

  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}  -- We recommend updating the parsers on update Highlight, edit, and navigate code
   use { 'nvim-treesitter/nvim-treesitter-textobjects', after = { 'nvim-treesitter' } }-- Additional textobjects for treesitter
   use 'nvim-treesitter/nvim-tree-docs'
  --use 'nvim-lua/completion-nvim'
  use 'udalov/kotlin-vim'
  --use 'norcalli/snippets.nvim'
  use { 'hrsh7th/nvim-cmp', requires = { 'hrsh7th/cmp-nvim-lsp' } }                    -- Autocompletion
  use { 'L3MON4D3/LuaSnip', requires = { 'saadparwaiz1/cmp_luasnip' } }               --snippet engine and snippets expansion
  use 'rafamadriz/friendly-snippets'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  use 'hrsh7th/cmp-cmdline'
  --use 'yamatsum/nvim-nonicons'
  use {'kyazdani42/nvim-tree.lua', requires = {'kyazdani42/nvim-web-devicons'}} -- for file icons

  use 'hrsh7th/vim-vsnip-integ'
  use 'hrsh7th/cmp-vsnip'
  use 'hrsh7th/vim-vsnip'
  --use 'github/copilot.vim'
  --use { 'subnut/nvim-ghost.nvim', run = function() vim.fn['nvim_ghost#installer#install()'](0) end }
  use {
    'glacambre/firenvim',
    run = function() vim.fn['firenvim#install'](0) end
}

--use {'Vaisakhkm2625/hologram-math-preview.nvim'}
use {'~/dev/neovim-plugins/hologram-math-preview.nvim', branch= 'show-all-eq-fix'}

--use {'edluffy/hologram.nvim'} -- to see images inside neovim
use {
  'vhyrro/hologram.nvim',
  config = function()
    require("hologram").setup()
  end,

  --rocks = {"magick"},
}
-- to see images inside neovim
use {'3rd/image.nvim'} -- next level of images in neovim
  -- color highlither for css
  use 'norcalli/nvim-colorizer.lua'
  --use 'ap/vim-css-color'
  use {
  'phaazon/hop.nvim',
  branch = 'v1', -- optional but strongly recommended
  config = function()
    -- you can configure Hop the way you like here; see :h hop-config
    require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
  end
}
-- use {
--   'rcarriga/nvim-notify',
--   config = function()
--     require('notify').setup ({ background_colour = "#000000"})
--   end
-- }
--use 'rcarriga/nvim-notify'


-- use 'MunifTanjim/nui.nvim'
-- use 'folke/noice.nvim'
end)
