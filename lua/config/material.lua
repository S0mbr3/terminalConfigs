--require'material'.setup()
--vim.g.material_style = "palenight"
--Lua:
vim.api.nvim_set_keymap('n', '<leader>mm', [[<Cmd>lua require('material.functions').toggle_style()<CR>]], { noremap = true, silent = true })
--Lua:
vim.api.nvim_set_keymap('n', '<leader>ml', [[<Cmd>lua require('material.functions').change_style('lighter')<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>md', [[<Cmd>lua require('material.functions').change_style('darker')<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>mp', [[<Cmd>lua require('material.functions').change_style('palenight')<CR>]], { noremap = true, silent = true })

