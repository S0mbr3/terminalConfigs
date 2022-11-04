
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
