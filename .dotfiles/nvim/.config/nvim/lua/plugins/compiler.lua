return {
  { -- This plugin
    "Zeioth/compiler.nvim",
    cmd = {"CompilerOpen", "CompilerToggleResults", "CompilerRedo"},
    keys = {{"<F6>", "<cmd>CompilerOpen<cr>", desc="[O]pen [C]ompiler [O]ptions"},
      {"<Leader>cr",
        -- "<cmd>CompilerStop<cr>"..
        "<cmd>CompilerRedo<cr>", desc="[R]edo [L]ast [S]election [O]ption"},
      {"<S-F7>", "<cmd>CompilerToggleResults<cr>", desc="[T]oggle [C]ompiler [R]esult"},},
    dependencies = { "stevearc/overseer.nvim" },
    opts = {},
  },
  { -- The task runner we use
    "stevearc/overseer.nvim",
    commit = "6271cab7ccc4ca840faa93f54440ffae3a3918bd",
    cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
    opts = {
      task_list = {
        direction = "bottom",
        min_height = 25,
        max_height = 25,
        default_detail = 1
      },
    },
  }
}
