--local execute = vim.api.nvim_command
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

require('plugins')
require('config')
require('utils')
--require('config/nvim-tata')
--for f in fn.glob('~/.config/nvim/configs/lua/plugins/*lua', 0, 1) do
--execute 'require(${f})'
--require(string.format('%s', f))
--end
	-- My plugins here
	-- use 'foo1/bar1.nvim'
	-- use 'foo2/bar2.nvim'

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end


-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})


vim.lsp.set_log_level("debug")
--vim.api.nvim_set_keymap('n', '<Leader>lg', ":LazyGit<cr>", { noremap = true, silent = true })
--vim.api.nvim_set_keymap('n', '<Leader>lc', ":LazyGitConfig<cr>", { noremap = true, silent = true })

