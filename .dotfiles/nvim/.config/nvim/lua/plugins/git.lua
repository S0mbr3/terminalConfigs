return {
  {'tpope/vim-surround'}, --useging that allow to surround text
  {'tpope/vim-fugitive'}, --Git wrapper for vim
  {'tpope/vim-rhubarb'},                                                              -- Fugitive-companion to interact with github
  { 'lewis6991/gitsigns.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },            -- Add git related info in the signs columns and popups
  {'kdheepak/lazygit.nvim',
    config = function()
      vim.api.nvim_set_keymap('n', '<Leader>gg', ":LazyGit<cr>", { noremap = true, silent = true })
      vim.api.nvim_set_keymap('n', '<Leader>gc', ":LazyGitConfig<cr>", { noremap = true, silent = true })
    end
  }
}
