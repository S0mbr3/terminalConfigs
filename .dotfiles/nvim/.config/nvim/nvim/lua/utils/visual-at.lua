function execute_macro_over_visual_range()
  local macro_key = vim.fn.nr2char(vim.fn.getchar())
  print("@", macro_key)
  vim.api.nvim_command("'<,'>normal @" .. macro_key)
end

vim.api.nvim_set_keymap('x', '@', [[:lua execute_macro_over_visual_range()<CR>]], { noremap = true })
