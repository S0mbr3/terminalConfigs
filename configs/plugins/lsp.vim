lua << EOF
--lspconfig = require'lspconfig'
--require('.config.nvim.configs.lua.lsp.lua')
USER = vim.fn.expand('$USER')
local
function dofile (filename)
      local f = assert(loadfile(filename))
      return f()
    end
dofile("/home/" .. USER .."/.config/nvim/configs/lua/lsp/lua.lua")
require'lspconfig'.kotlin_language_server.setup{cmd = {"kotlin-language-server"}}
require'lspconfig'.pyright.setup{}
require'lspconfig'.ccls.setup{}
require'lspconfig'.bashls.setup{}
require'lspconfig'.dockerls.setup{}
require'lspconfig'.cmake.setup{}
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
require'lspconfig'.html.setup { capabilities = capabilities}
require'lspconfig'.tsserver.setup{}
--require'lspconfig'.denols.setup{on_attach=require'completion'.on_attach}
--require'lspconfig'.jedi_language_server.setup{}
EOF
