vim.cmd([[ let g:nvim_tree_special_files = { 'README.md': 1, 'Makefile': 1, 'MAKEFILE': 1 } ]]) -- List of filenames that gets highlighted with NvimTreeSpecialFile
vim.api.nvim_set_keymap('n', '<Leader>ne', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>r', ':NvimTreeRefresh<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>n', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })
--vim.g.nvim_tree_disable_window_picker=1
--vim.cmd([[ highlight NvimTreeFolderIcon guibg=blue]])
require'nvim-tree'.setup{}
