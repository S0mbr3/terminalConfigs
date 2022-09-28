vim.g.mapleader = ' ' --set the mapleader to space

--visual mapping
vim.api.nvim_set_keymap('v', '<C-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-l>', '<C-w>l', { noremap = true, silent = true })

--normal mapping
vim.api.nvim_set_keymap('n', '<C-n>', ':noh<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader><Space>', ':set hlsearch!<CR>', { noremap = true, silent = true })

--insert mapping
vim.api.nvim_set_keymap('i', '<C-h>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-j>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-k>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-l>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })


--terminal mapping
vim.api.nvim_set_keymap('t', '<C-h>', '<C-\\><C-n><C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<C-j>', '<C-\\><C-n><C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<C-k>', '<C-\\><C-n><C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<C-l>', '<C-\\><C-n><C-w>l', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<Leader><Esc>', '<C-\\><C-n>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>o', ':below 10sp term://$SHELL<cr>i', { noremap = true, silent = false })

--leader mapping
--Allows to save a file that need sudo permissions
vim.api.nvim_set_keymap('n', '<Leader>ws', ':w !sudo tee %<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>wa', ':wa<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>wqa', ':wqa<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>qf', ':q!<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>qaf', ':qa!<CR>', { noremap = false, silent = true })

-- 'cd' towards the directory in which the current file is edited
-- but only change the path for the current window
vim.api.nvim_set_keymap('n', '<Leader>cd', ':lcd %:h<CR>', { noremap = true, silent = false })


