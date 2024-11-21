vim.cmd([[ au FocusGained * :checktime ]]) --when vim can back the focus apply checktime which load back the file it's modified
--vim.cmd([[ autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif ]]) --Auto close the deoplete preview window when completion is done
--autocmd BufEnter * lua require'completion'.on_attach() --Enable auto completion for all buffer into LSP using completion-nvim
--
-- Autocmd to compile and run c or c++ program after a buf save
-- vim.api.nvim_create_autocmd({"BufWritePost"}, {
--   pattern = {"*.c", "*.cpp"},
--   --command = "echo 'saving a C or C++ file'",
--   command = "make && time ./%:p:h:t",
-- })

vim.api.nvim_create_autocmd({"FileType"},{
  group = vim.api.nvim_create_augroup("formatoptions", { clear = true }),
  pattern = {"*"},
  callback = function(_)
    vim.opt.formatoptions:remove {'r', 'o'}
    --vim.opt.shortmess="aA"
  end
})

--[[ vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "Caddyfile",
  command = "set filetype=caddyfile"
}) ]]
