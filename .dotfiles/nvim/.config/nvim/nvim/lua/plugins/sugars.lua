return {
  {'nvim-tree/nvim-tree.lua',
  dependencies = {'nvim-tree/nvim-web-devicons'},
  config = function()
    --vim.cmd([[ let g:nvim_tree_special_files = { 'README.md': 1, 'Makefile': 1, 'MAKEFILE': 1 } ]]) -- List of filenames that gets highlighted with NvimTreeSpecialFile
    vim.api.nvim_set_keymap("n", '<Leader>nl', '<cmd>lua require("nvim-tree.api").marks.list()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", '<Leader>nn', '<cmd>lua require("nvim-tree.api").marks.navigate.next()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", '<Leader>np', '<cmd>lua require("nvim-tree.api").marks.navigate.prev()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", '<Leader>ns', '<cmd>lua require("nvim-tree.api").marks.navigate.select()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>ne', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
    --vic.api.nvim_set_keymap('n', '<Leader>r', ':NvimTreeRefresh<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>nf', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })
    --vim.g.nvim_tree_disable_window_picker=1
    --vim.cmd([[ highlight NvimTreeFolderIcon guibg=blue]])
    require'nvim-tree'.setup({
      renderer = {
        special_files = { "Cargo.toml", "Makefile", "README.md", "readme.md", "MAKEFILE" },
      }
    })

  end
}, -- for file icons

{'numToStr/Comment.nvim',
opts = {
  -- add any options here
},
lazy = false,
},
{'lambdalisue/suda.vim'},
{'brooth/far.vim'}, --asynchronous search and replace operations on a set of files
{'Shougo/echodoc.vim'}, --print completed documention
{'Shougo/context_filetype.vim'}, --add context filype feature
{'tpope/vim-surround'}, --useging that allow to surround text
{ 's1n7ax/nvim-terminal',
config = function()
  -- following option will hide the buffer when it is closed instead of deleting
  -- the buffer. This is important to reuse the last terminal buffer
  -- IF the option is not set, plugin will open a new terminal every single time
  vim.o.hidden = true

  require('nvim-terminal').setup({
    window = {
      -- Do `:h :botright` for more information
      -- NOTE: width or height may not be applied in some "pos"
      position = 'botright',

      -- Do `:h split` for more information
      split = 'sp',

      -- Width of the terminal
      width = 50,

      -- Height of the terminal
      height = 15,
    },

    -- keymap to disable all the default keymaps
    disable_default_keymaps = false,

    -- keymap to toggle open and close terminal window
    toggle_keymap = '<leader>;',

    -- increase the window height by when you hit the keymap
    window_height_change_amount = 2,

    -- increase the window width by when you hit the keymap
    window_width_change_amount = 2,

    -- keymap to increase the window width
    increase_width_keymap = '<leader><leader>+',

    -- keymap to decrease the window width
    decrease_width_keymap = '<leader><leader>-',

    -- keymap to increase the window height
    increase_height_keymap = '<leader>+',

    -- keymap to decrease the window height
    decrease_height_keymap = '<leader>-',

    terminals = {
      -- keymaps to open nth terminal
      {keymap = '<leader><C-1>'},
      {keymap = '<leader><C-2>'},
      {keymap = '<leader><C-3>'},
      {keymap = '<leader><C-4>'},
      {keymap = '<leader><C-5>'},
    },
  })
end,
  },

  {
    'phaazon/hop.nvim',
    branch = 'v1', -- optional but strongly recommended
    config = function()
      -- you can configure Hop the way you like here; see :h hop-config
      require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
      vim.api.nvim_set_keymap('n', '<leader>hh', ':HopWord<CR>', { noremap = true, silent = true })
      vim.api.nvim_set_keymap('n', '<leader>e', ':HopChar1<CR>', { noremap = true, silent = true })
      vim.api.nvim_set_keymap('n', '<leader>hp', ':HopPattern<CR>', { noremap = true, silent = true })
      vim.api.nvim_set_keymap('n', '<leader>hw', ':HopChar2<CR>', { noremap = true, silent = true })
    end
  },

  {'ThePrimeagen/harpoon',
  config = function()
    vim.api.nvim_set_keymap('n', '<Leader>p', ':lua require("harpoon.mark").add_file()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>H', ':lua require("harpoon.ui").toggle_quick_menu()<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>1', ':lua require("harpoon.ui").nav_file(1)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>2', ':lua require("harpoon.ui").nav_file(2)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>3', ':lua require("harpoon.ui").nav_file(3)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>4', ':lua require("harpoon.ui").nav_file(4)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>5', ':lua require("harpoon.ui").nav_file(5)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>6', ':lua require("harpoon.ui").nav_file(6)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>7', ':lua require("harpoon.ui").nav_file(7)<CR>', { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<Leader>8', ':lua require("harpoon.ui").nav_file(8)<CR>', { noremap = true, silent = true })

  end,
},
{'nvim-pack/nvim-spectre',
dependencies = {'nvim-lua/plenary.nvim'},
config = function()
  vim.keymap.set('n', '<leader>S', '<cmd>lua require("spectre").toggle()<CR>', {
    desc = "Toggle Spectre"
  })
  vim.keymap.set('n', '<leader>sw', '<cmd>lua require("spectre").open_visual({select_word=true})<CR>', {
    desc = "Search current word"
  })
  vim.keymap.set('v', '<leader>sw', '<esc><cmd>lua require("spectre").open_visual()<CR>', {
    desc = "Search current word"
  })
  vim.keymap.set('n', '<leader>sp', '<cmd>lua require("spectre").open_file_search({select_word=true})<CR>', {
    desc = "Search on current file"
  })
end
},
{
  'mikesmithgh/kitty-scrollback.nvim',
  enabled = true,
  lazy = true,
  cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth' },
  -- version = '*', -- latest stable version, may have breaking changes if major version changed
  -- version = '^1.0.0', -- pin major version, include fixes and features that do not have breaking changes
  config = function()
    require('kitty-scrollback').setup()
  end,
}
}
