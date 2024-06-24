--use 'norcalli/snippets.nvim'completion
return {
  { 'L3MON4D3/LuaSnip',
    dependencies = { 'saadparwaiz1/cmp_luasnip', 'rafamadriz/friendly-snippets' },
    event = "InsertEnter",
    build = "make install_jsregexp",
  },--snippet engine and snippets expansion
  {'hrsh7th/nvim-cmp', version = false, dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'hrsh7th/cmp-cmdline',
    'windwp/nvim-autopairs',
    'hrsh7th/vim-vsnip-integ',
    'hrsh7th/cmp-vsnip',
    'hrsh7th/cmp-emoji',
    'hrsh7th/vim-vsnip',
    'L3MON4D3/LuaSnip'
  },
    event = "InsertEnter",
    config = function()
      -- Setup nvim-cmp.
      local cmp_autopairs = require('nvim-autopairs.completion.cmp')
      local cmp = require'cmp'
      local luasnip = require'luasnip'
      require("luasnip.loaders.from_vscode").lazy_load()
      --require("luasnip/loaders/from_vscode").load { paths = { "~/.config/lvim/snippets/vscode-es7-javascript-react-snippets" } }
      luasnip.filetype_extend("php", {"html"})
      --luasnip.filetype_extend("javascript", {"javascriptreact"})
      luasnip.filetype_extend("javascript", {"html"})
      -- luasnip.filetype_extend("javascriptreact", {"javascript"})
      -- luasnip.filetype_extend("typescriptreact", {"typescript"})

      cmp.event:on( 'confirm_done', cmp_autopairs.on_confirm_done({  map_char = { tex = '' } }))


      -- add a lisp filetype (wrap my-function), FYI: Hardcoded = { "clojure", "clojurescript", "fennel", "janet" }
      --cmp_autopairs.lisp[#cmp_autopairs.lisp+1] = "lua"


      cmp.setup({
        matching = {
          disallow_fuzzy_matching = false,
          disallow_fullfuzzy_matching = false,
          disallow_partial_fuzzy_matching = true,
          disallow_partial_matching = false,
          disallow_prefix_unmatching = false,
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            --vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
            -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
          end,
        },
        --[[mapping = {
      ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<Tab>'] = cmp.mapping.confirm({ select = true }),
      ["<C-j>"] = cmp.mapping.select_next_item(),
      ["<C-k>"] = cmp.mapping.select_prev_item(),
    },--]]
        mapping = cmp.mapping.preset.insert {
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          },
          ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { 'i', 's' }),
        },
        completion = {
          completeopt="menu,noinsert,noselect"
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          --{ name = 'vsnip' }, -- For vsnip users.
          { name = 'luasnip' }, -- For luasnip users.
          -- { name = 'ultisnips' }, -- For ultisnips users.
          -- { name = 'snippy' }, -- For snippy users.
        }, {
            { name = 'buffer' },
            { name = 'path'},
            { name = 'cmp-cmdline'},
            { name = "neorg"}
          })
      })

      -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({'/', '?'}, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
            { name = 'cmdline' },
            option = {
              ignore_cmds = {'Man', '!'}
            }
          })
      })

    end
  },
  { "ibhagwan/fzf-lua",
    enabled = vim.fn.has("unix"),
    -- optional for icon support
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "InsertEnter",
    config = function()
      -- calling `setup` is optional for customization
      require("fzf-lua").setup({})
    end
  },                    -- Autocompletion
  {'windwp/nvim-autopairs',
    event = "InsertEnter",
    opts = {} -- this is equalent to setup ({}) function
  },
  {'junegunn/fzf',
    name = 'fzf',
    dir ='~/.fzf',
    build = './install --all',
    enabled = vim.fn.has("unix"),
    event = "VeryLazy"
  }

  -- {'junegunn/fzf',
  -- build = './install --bin',
  -- enabled = vim.fn.has("unix")
  -- },
  --{'junegunn/fzf.vim'},
}
