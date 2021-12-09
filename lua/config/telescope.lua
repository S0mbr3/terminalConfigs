vim.api.nvim_set_keymap('n', '<Leader>ff', "<cmd>lua require('telescope.builtin').find_files()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", { noremap = true, silent = true })


