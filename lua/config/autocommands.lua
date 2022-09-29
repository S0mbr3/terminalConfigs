vim.cmd([[ set autoread ]]) --detetec when the files have been change outside of vim or by another buffer of this file
vim.cmd([[ au FocusGained * :checktime ]]) --when vim can back the focus apply checktime which load back the file it's modified
--vim.cmd([[ autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif ]]) --Auto close the deoplete preview window when completion is done
--autocmd BufEnter * lua require'completion'.on_attach() --Enable auto completion for all buffer into LSP using completion-nvim

