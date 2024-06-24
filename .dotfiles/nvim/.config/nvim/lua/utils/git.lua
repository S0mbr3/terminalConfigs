function git_commit()
  local message = vim.fn.input('Enter commit message: ')
  os.execute("git commit -m '" .. message .. "'")
end

-- git add buffer / add to staging area
vim.api.nvim_set_keymap('n', '<leader>ga', [[!git add %<CR>]], { noremap = true })

-- git reset buffer / lossless unstage
vim.api.nvim_set_keymap('n', '<leader>gr', [[!git reset %<CR>]], { noremap = true })

-- git commit
vim.api.nvim_set_keymap('n', '<leader>gc', [[<Cmd>lua git_commit()<CR>]], { noremap = true })

-- git push
vim.api.nvim_set_keymap('n', '<leader>gp', [[!git push<CR>]], { noremap = true })
