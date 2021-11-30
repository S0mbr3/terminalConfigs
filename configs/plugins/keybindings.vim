let mapleader = " " "using space as leader key

"visual mapping
xnoremap <C-h> <C-w>h
xnoremap <C-j> <C-w>j
xnoremap <C-k> <C-w>k
xnoremap <C-l> <C-w>l

"normal mapping
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

"insert mapping
inoremap <C-h> <C-\><C-n><C-w>h
inoremap <C-j> <C-\><C-n><C-w>j
inoremap <C-k> <C-\><C-n><C-w>k
inoremap <C-l> <C-\><C-n><C-w>l

"terminal mapping
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <Esc> <C-\><C-n> 

"leader remaping
"Allows to save a file that need sudo permissions
nmap <Leader>ws :w !sudo tee %<CR>
nmap <Leader>w :w<CR>
nmap <Leader>wa :wa<CR>
nmap <Leader>wqa :wqa<CR>
nmap <Leader>q :q<CR>
nmap <Leader>qf :q!<CR>
nmap <Leader>qaf :qa!<CR>
nmap <Leader>mm :!./makefile<CR>
nmap <Leader>m :w<CR><C-j>A./makefile<CR>./main<CR><C-k>
nnoremap <leader>t :below 15sp term://$SHELL<cr>i
