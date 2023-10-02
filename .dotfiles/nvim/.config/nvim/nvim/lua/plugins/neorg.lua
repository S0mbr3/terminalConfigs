return  {
  "nvim-neorg/neorg",
  build = ":Neorg sync-parsers",
  dependencies = { "nvim-lua/plenary.nvim", 'hrsh7th/nvim-cmp' },
  config = function()
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
  end,
}
