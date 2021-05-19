--local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
end

require('plugins')
for f in fn.glob('~/.config/nvim/configs/lua/plugins/*lua', 0, 1) do
  --execute 'require(${f})'
  require(string.format('%s', f))
end
