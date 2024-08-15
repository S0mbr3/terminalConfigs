local colorscheme = "material"
return {
  --{'morhetz/gruvbox'}
  {
    "scottmckendry/cyberdream.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      transparent=true,
      variant='light'
    }
  },
  { 'catppuccin/nvim', name = 'catppuccin', enabled=true, priority=1000, lazy=false,
  opts = {
      transparent_background = true
  },
},
{'folke/tokyonight.nvim',enabled=true, lazy=false,priority = 1000,
opts = {
    transparent=false,
    terminal_colors=true,
    dim_inactive=true,
},
},
{"tiagovla/tokyodark.nvim",enabled=true,lazy=false, priority = 1000,
opts = {
  transparent_background = true

},
config = function(_, opts)
  require("tokyodark").setup(opts)
  vim.cmd.colorscheme(colorscheme)
end
},
{'marko-cerovac/material.nvim',
enabled=true,
dependencies = {  'nvim-lualine/lualine.nvim'},
lazy = false,
priority = 1000,
opts = {

},
config = function()
  require'material'.setup({
    disable={
      background=not vim.g.started_by_firenvim
    },
    plugins = { -- Uncomment the plugins that you use to highlight them
      -- Available plugins:
      -- "dap",
      -- "dashboard",
      -- "gitsigns",
      "hop",
      -- "indent-blankline",
      "lspsaga",
      -- "mini",
      -- "neogit",
      "nvim-cmp",
      -- "nvim-navic",
      "nvim-tree",
      -- "sneak",
      "telescope",
      -- "trouble",
      -- "which-key",
    },
    lualine_style="stealth",
  })
  vim.g.material_style = 'palenight'
  vim.api.nvim_set_keymap('n', '<leader>mm', [[<Cmd>lua require('material.functions').toggle_style()<CR>]], { noremap = true, silent = true })
  --Lua:
  vim.api.nvim_set_keymap('n', '<leader>ml', [[<Cmd>lua require('material.functions').change_style('lighter')<CR>]], { noremap = true, silent = true })
  vim.api.nvim_set_keymap('n', '<leader>md', [[<Cmd>lua require('material.functions').change_style('darker')<CR>]], { noremap = true, silent = true })
  vim.api.nvim_set_keymap('n', '<leader>mp', [[<Cmd>lua require('material.functions').change_style('palenight')<CR>]], { noremap = true, silent = true })
end,
},
{'owozsh/amora',enabled=false},
{'ellisonleao/gruvbox.nvim', dependencies = {'rktjmp/lush.nvim'},enabled=false},
{'nvim-lualine/lualine.nvim',
dependencies = {
  'nvim-tree/nvim-web-devicons',
  'meuter/lualine-so-fancy.nvim'},
  opt = true,
  config = function()
    require('lualine').setup {
      options = {
        -- ... your lualine config
        --theme = 'material-nvim'
        theme = 'auto'
        -- ... your lualine config
      },
      sections = {
        lualine_a = {
          { "fancy_mode", width = 3 }
        },
        lualine_b = {
          { "fancy_branch" },
          { "fancy_diff" },
        },
        lualine_c = {
          { "fancy_cwd", substitute_home = true }
        },
        lualine_x = {
          { "fancy_macro" },
          { "fancy_diagnostics" },
          { "fancy_searchcount" },
          { "fancy_location" },
        },
        lualine_y = {
          { "fancy_filetype", ts_icon = "" }
        },
        lualine_z = {
          { "fancy_lsp_servers" }
        },
      }
    }
    end
  },
  config = function()

    --vim.cmd[[colorscheme tokyonight-moon]]
    --vim.cmd[[colorscheme catppuccin-mocha]]
    --vim.cmd[[colorscheme material]]
    --vim.cmd[[colorscheme tokyodark]]
    --vim.cmd.colorscheme"tokyodark"
    --vim.cmd [[colorscheme tokyonight-night]]
  end
}
