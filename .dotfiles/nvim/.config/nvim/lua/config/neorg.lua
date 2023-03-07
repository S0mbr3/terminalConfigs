require('neorg').setup {
  load = {
    ["core.defaults"] = {},
    ["core.norg.dirman"] = {
      config = {
        workspaces = {
          work = "~/notes/work",
          home = "~/notes/home",
        }
      }
    },
    ['core.norg.completion'] = {
      config = {
        engine = "nvim-cmp",
      }
    },
    ['core.norg.concealer'] = {
      config = {
        icon_preset = "diamond",
        folds = false,
        dim_code_blocks = {
          enabled = true,
          content_only = true
        }
      }
    },
  }
}
