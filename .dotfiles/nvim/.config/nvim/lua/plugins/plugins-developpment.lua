return {
  "folke/neodev.nvim", opts = {},
  config = function()

    vim.api.nvim_set_keymap('n', '<leader>po', ':lua require("nabla").popup()<CR>' , { noremap = true, silent = true })
    vim.api.nvim_set_keymap('n', '<leader>pv', ':lua require("nabla").toggle_virt({autogen=true})<CR>' , { noremap = true, silent = true })
  end,
}
