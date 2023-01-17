function GitCommit() abort
  let message = input('Enter commit message: ')
  call system("git commit -m '" . message . "'")
  endfunction

  " git add buffer / add to staging area
  nnoremap <leader>ga :!git add %<CR>
  " git reset buffer / lossless unstage
  nnoremap <leader>gr :!git reset %<CR>
  " git commit
  nnoremap <leader>gc :call GitCommit()<CR>
  " git push
  nnoremap <leader>gp :!git push<CR>
