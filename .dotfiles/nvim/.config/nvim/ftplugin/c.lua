local keymap = vim.keymap.set
local compile = function()
  vim.api.nvim_command('w')
  vim.api.nvim_command("lua os.execute('~/scripts/bin/compile')")
end
-- keymap("n", "\\j", "<cmd>:w | lua os.execute('~/scripts/bin/compile')<CR>")
keymap("n", "\\j", compile, {silent=true})
