--vim.g.material_transparent_sidebar = true
--vim.g.material_transparent = true
vim.opt.background = "dark"
require("tokyonight").setup({
  transparent=true,
  terminal_colors=true,
  dim_inactive=true,
})



-- settings for material colorscheme
require'material'.setup({
  disable={
    background=true
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
vim.g.material_style = "palenight"
--Lua:
--Lua:
vim.api.nvim_set_keymap('n', '<leader>mm', [[<Cmd>lua require('material.functions').toggle_style()<CR>]], { noremap = true, silent = true })
--Lua:
vim.api.nvim_set_keymap('n', '<leader>ml', [[<Cmd>lua require('material.functions').change_style('lighter')<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>md', [[<Cmd>lua require('material.functions').change_style('darker')<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>mp', [[<Cmd>lua require('material.functions').change_style('palenight')<CR>]], { noremap = true, silent = true })

-- -- Enable `lukas-reineke/indent-blankline.nvim`
--See `:help indent_blankline.txt`

require("tokyodark").setup({
  transparent_background = true
})

require("catppuccin").setup({
  transparent_background = true
})


--vim.cmd[[colorscheme catppuccin-mocha]]
--vim.cmd[[colorscheme tokyonight-moon]]
--vim.cmd[[colorscheme tokyonight-night]]
--vim.cmd[[colorscheme material]]
vim.cmd[[colorscheme tokyodark]]

if (vim.g.colors_name == 'tokyonight-night' or vim.g.colors_name == 'tokyodark') then
  vim.o.cursorline=false
  -- Set a bunch of things
end
