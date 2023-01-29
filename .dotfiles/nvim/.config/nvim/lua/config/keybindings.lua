-- leader mapping has to come before requiring plugins !
vim.g.mapleader = ' ' --set the mapleader to spaceswitchterm
vim.g.maplocalleader = ' '

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

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
--vim.api.nvim_set_keymap('t', '<Leader><Esc>', '<C-\\><C-n>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<C-space>', '<C-\\><C-n>', { noremap = true, silent = true })
vim.keymap.set({'t', 'n', 'i', 'v' }, '<M-s>', '<cmd>lua require("utils.switchterm").switchTerm()<CR>', { noremap = true, silent = true })
vim.keymap.set('t' , '<C-s>', '<cmd>lua require("utils.switchterm").termSizer()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>o', ':below 10sp term://$SHELL<cr>i', { noremap = true, silent = false })

--leader mapping
--Allows to save a file that need sudo permissions
vim.api.nvim_set_keymap('n', '<Leader>w', ':w<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>sa', ':wa<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>sqa', ':wqa<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>qf', ':q!<CR>', { noremap = false, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>qaf', ':qa!<CR>', { noremap = false, silent = true })

vim.api.nvim_set_keymap('n', '<Leader>x', ':w|luafile %<CR>', { noremap = false, silent = true })

-- 'cd' towards the directory in which the current file is edited
-- but only change the path for the current window
vim.api.nvim_set_keymap('n', '<Leader>cd', ':lcd %:p:h<CR>', { noremap = true, silent = false })

-- switching tab keybinding
vim.keymap.set({'n', 'i', 'v' }, '<M-1>', '1gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-2>', '2gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-3>', '3gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-4>', '4gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-5>', '5gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-6>', '6gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-7>', '7gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-8>', '8gt', { noremap = true, silent = true })
vim.keymap.set({'n', 'i', 'v' }, '<M-9>', '9gt', { noremap = true, silent = true })

-- for terminal tab switching
vim.api.nvim_set_keymap('t', '<M-1>', '<C-\\><C-n>1gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-2>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-3>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-4>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-5>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-6>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-7>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-8>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
vim.api.nvim_set_keymap('t', '<M-9>', '<C-\\><C-n>2gt', { noremap = true, silent = true })
