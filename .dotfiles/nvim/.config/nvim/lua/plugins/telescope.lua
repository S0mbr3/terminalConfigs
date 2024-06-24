local actions = require("telescope.actions")
local builtin = require("telescope.builtin")
local extensions = require("telescope").extensions

local rg_args = {"-.","-g", '!**/node_modules/*', "-g", '!**/.git/*'}

return {
  -- {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
  -- {'nvim-telescope/telescope-project.nvim'},
  -- { "nvim-telescope/telescope-file-browser.nvim" },
  {'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-project.nvim',
      'nvim-telescope/telescope-file-browser.nvim',
      {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
    },
    opts = {
      pickers = {
        find_files = {
          find_command = { "rg", "--ignore", "-L", "--hidden", "--files", "--glob", "!node_modules/*", "--glob", "!.git/*"}
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
    },
    keys = {
      {'<leader>fa',
        function () builtin.find_files{cwd = "~/Documents/builds/terminalConfigs/.dotfiles/"} end,
      desc = '[S]earch [F]iles relative'},

      {'<leader>fr',
        function () builtin.live_grep{cwd = "~/Documents/builds/terminalConfigs/.dotfiles/", additional_args=rg_args} end,
      desc = "[S]earch [Hiden] [Grep]"},

      {'<leader>fd', builtin.diagnostics, desc= '[S]earch [D]iagnostics'},

      {'<leader>fo', builtin.oldfiles, desc = '[S]earch [R]ecently [O]ened [F]iles'},

      {'<leader>ff', function() builtin.find_files() end, desc = "[F]ind [Files] in current directory"},
      {'<leader>fg', function() builtin.live_grep() end, desc = "[G]rep in current directory"},
      {'<leader>fb', function() builtin.buffers() end, desc = "[S]earch opened [Buffers]"},
      {'<leader>fh', function() builtin.help_tags() end, desc = "[Search] items in the nvim [Help]"},
      {'<leader>fc', function() extensions.project.project{} end},
      {'<leader>fc', function() extensions.file_browser.file_browser{} end},
      {'<leader>fe', function() builtin.builtin({include_extensions=true})end}

    },
  config = function(_, opts)
      require'telescope'.setup(opts)
    -- To get fzf loaded and working with telescope, you need to call
    -- load_extension, somewhere after setup function:
    require('telescope').load_extension('project')
    require('telescope').load_extension('harpoon')
    require('telescope').load_extension('file_browser')
    require('telescope').load_extension('fzf')

  end
},
}
