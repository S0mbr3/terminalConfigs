local keymap = vim.keymap.set
vim.api.nvim_set_keymap('n', 'gh', "<cmd>Lspsaga lsp_finder<CR>", { noremap = true, silent = true })
keymap({"n","v"}, "<leader>ca", "<cmd>Lspsaga code_action<CR>", { silent = true })
-- vim.api.nvim_set_keymap('n', 'ca', "<cmd>lua require('lspsaga.codeaction').code_action()<CR>", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('v', 'ca', ":<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'K', "<cmd>Lspsaga hover_doc<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-f>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-b>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gs', "<cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>lr', "<cmd>Lspsaga rename<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>ld', "<cmd>lua require('lspsaga.provider').preview_definition()<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>cd', "<cmd>Lspsaga show_line_diagnostics<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>cc', "<cmd>Lspsaga show_cursor_diagnostics<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '[e', "<cmd>Lspsaga diagnostic_jump_prev<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', ']e', "<cmd>Lspsaga diagnostic_jump_next<CR>", { noremap = true, silent = true })
-- Only jump to error
vim.keymap.set("n", "[E", function()
  require("lspsaga.diagnostic").goto_prev({ severity = vim.diagnostic.severity.ERROR })
end, { silent = true })
vim.keymap.set("n", "]E", function()
  require("lspsaga.diagnostic").goto_next({ severity = vim.diagnostic.severity.ERROR })
end, { silent = true })
--vim.api.nvim_set_keymap('n', '<A-d>', "<cmd>Lspsaga term_floaterm<CR>", { noremap = true, silent = true })
--vim.api.nvim_set_keymap('t', '<A-d>', "<C-\\><C-n><cmd>Lspsaga close_floaterm<CR>", { noremap = true, silent = true })
-- Float terminal
keymap({"n", "t"}, "<A-q>", "<cmd>Lspsaga term_toggle<CR>")

keymap("n", "<leader>lp", "<cmd>Lspsaga peek_definition<CR>", { silent = true })
-- outline
keymap("n","<leader>lo", "<cmd>LSoutlineToggle<CR>",{ silent = true })

keymap("n", "gp", "<cmd>Lspsaga peek_definition<CR>")

-- Go to Definition
keymap("n","gd", "<cmd>Lspsaga goto_definition<CR>")
--end of LSPSAGA CONFIGURATION

-- Diagnostic keymaps
--[[vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)
--]]
-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions

 vim.diagnostic.config({
        virtual_text = true,
        signs = true,
        underline = true,
        update_in_insert = true,
        severity_sort = true,
    })

local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  --nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  --nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
  nmap('gi', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
  nmap('gr', require('telescope.builtin').lsp_references, '[T]elescope [R]eferences')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  --nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>D', vim.lsp.buf.type_definition, 'Type [D]efinition')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<Leader>lf', '<cmd>:Format<CR>', '[F]ormat whole buffer')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    if vim.lsp.buf.format then
      vim.lsp.buf.format()
    elseif vim.lsp.buf.formatting then
      vim.lsp.buf.formatting()
    end
  end, { desc = 'Format current buffer with LSP' })
end

--vim.keymap.set('n', '<cmd>:Format<cr>', {buffer = bufnr, desc='LSP: [F]ormat whole buffer' })
-- nvim-cmp supports additional completion capabilities
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Enable the following language servers
local servers = { 'clangd', 'pyright', 'tsserver', 'lua_ls', 'angularls', 'bashls', 'phpactor', 'emmet_ls', "cssls", "eslint", "tailwindcss"}

-- Ensure the servers above are installed
require('mason-lspconfig').setup {
  ensure_installed = servers,
}

for _, lsp in ipairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

require'lspconfig'.emmet_ls.setup({
  on_attach = on_attach,
  capabilities = capabilities,
  filetypes = { "html", "php", "typescriptreact", "javascriptreact", "css", "sass", "scss", "less", "typescript"},
})

require'lspconfig'.lua_ls.setup {
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

-- Update this path
-- for rust debugger watch rust-tools
local extension_path = vim.env.HOME .. '/.vscode-oss/extensions/vadimcn.vscode-lldb-1.8.1-universal/'
local codelldb_path = extension_path .. 'adapter/codelldb'
local liblldb_path = extension_path .. 'lldb/lib/liblldb.so'  -- MacOS: This may be .dylib

local opts = {
  -- ... other configs
  dap = {
    adapter = require('rust-tools.dap').get_codelldb_adapter(
    codelldb_path, liblldb_path)
  }
}
local rt = require("rust-tools")

rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<Leader>r", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})

