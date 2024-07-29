vim.opt.conceallevel = 2 -- btw 0-3
vim.opt.concealcursor = "nc"
vim.o.foldexpr="nvim_treesitter#foldexpr()"
vim.o.foldmethod="expr"
vim.o.foldlevel=0
vim.o.showbreak=""
vim.o.scrolloff=999
-- more info
-- :h conceallevel
-- :h concealcursor

--require("zen-mode").toggle({
  --[[ window = {
    width = .50 -- width will be 85% of the editor width
  } ]]
--})
-- vim.cmd([[Goyo]])
