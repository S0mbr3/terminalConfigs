require('neorg').setup {
  load = {
    ["core.defaults"] = {},
    ["core.export"] = {},
    ["core.dirman"] = {
      config = {
        workspaces = {
          work = "~/notes/work",
          home = "~/notes/home",
        }
      }
    },
    ['core.completion'] = {
      config = {
        engine = "nvim-cmp",
      }
    },
    ['core.concealer'] = {
      config = {
        icon_preset = "diamond",
        folds = false,
        dim_code_blocks = {
          enabled = true,
          content_only = true
        }
      }
    },
    ['core.ui.calendar'] = {},
  }
}

vim.api.nvim_set_keymap('n', '<leader>po', ':lua require("nabla").popup()<CR>' , { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>pv', ':lua require("nabla").toggle_virt({autogen=true})<CR>' , { noremap = true, silent = true })

