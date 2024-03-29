local builtin = require("telescope.builtin")
local utils = require("telescope.utils")
-- ["<leader>fa"] = {function() builtin.find_files({cwd=utils.buffer_dir()}) end,
-- desc = "find files in cwd"}
vim.keymap.set('n', '<leader>fa', function () builtin.find_files{cwd = "~/Documents/builds/terminalConfigs/.dotfiles/"} end, { noremap = true, silent = true, desc = '[S]earch [F]files relative' })
vim.keymap.set('n', '<leader>fr', function () builtin.live_grep{cwd = "~/Documents/builds/terminalConfigs/.dotfiles/", additional_args={"-."}} end, { noremap = true, silent = true, desc = '[S]earch [H]iden [G]rep' })
vim.keymap.set('n', '<leader>fd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>fo', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.api.nvim_set_keymap('n', '<Leader>ff', "<cmd>lua require('telescope.builtin').find_files()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fg', "<cmd>lua require('telescope.builtin').live_grep()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fb', "<cmd>lua require('telescope.builtin').buffers()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fh', "<cmd>lua require('telescope.builtin').help_tags()<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fc', "<cmd>lua require'telescope'.extensions.project.project{}<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fi', "<cmd>lua require'telescope'.extensions.project.project{}<cr>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>fn', "<cmd>lua require'telescope'.extensions.file_browser.file_browser{}<cr>", { noremap = true, silent = true })


-- You dont need to set any of these options. These are the default ones. Only
vim.api.nvim_set_keymap('n', '<Leader>fe', "<cmd>lua require('telescope.builtin').builtin({ include_extensions=true })<cr>", { noremap = true, silent = true })
-- the loading is important
local actions = require("telescope.actions")
require('telescope').setup {
  pickers = {
    find_files = {
      find_command = { "rg", "--ignore", "-L", "--hidden", "--files"}
    }
  },
  defaults = {
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      },
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
    project = {
      hidden_files = true,
    },
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('project')
require('telescope').load_extension('harpoon')
require('telescope').load_extension('file_browser')
require('telescope').load_extension('fzf')
