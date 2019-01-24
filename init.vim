" Install Vim Plug if not installed
let mapleader = " "
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
set clipboard+=unnamedplus
call plug#begin()
Plug 'neomake/neomake'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
" Additional sources for Deoplete
Plug 'ludovicchabant/vim-gutentags'
"Plug 'jsfaint/gen_tags.vim'
Plug 'zchee/deoplete-jedi', {'for': 'python'} "sources for python
"Plug 'tweekmonster/deoplete-clang2' "sources for c/c++/objectiveC
Plug 'Rip-Rip/clang_complete', {'for': ['c', 'cpp', 'objectivec']}
Plug 'carlitux/deoplete-ternjs', { 'do': 'sudo npm install -g tern', 'for': ['javascript', 'javascript.jsx']  } "sources for javascript
"Plug 'pangloss/vim-javascript'
Plug 'othree/jspc.vim', { 'for': ['javascript', 'javascript.jsx']  }
Plug 'ternjs/tern_for_vim', { 'do': 'sudo npm install && sudo npm install -g tern', 'for': ['javascript', 'javascript.jsx']  }
Plug 'SevereOverfl0w/deoplete-github' "sources for gitcommit
Plug 'zchee/deoplete-asm', {'for': 'asm'} "sources for asm
Plug 'wellle/tmux-complete.vim' "sources for tmux panes
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'Quramy/tsuquyomi', { 'do': 'sudo npm install -g typescript' }
Plug 'mhartington/deoplete-typescript', {'for': ['typescript', 'tsx'], 'do': './install.sh'}
" End of additional deoplete sources

"Deoplete external plugins
Plug 'tpope/vim-surround' "Plugging that allow to surround text
Plug 'tpope/vim-fugitive' "Git wrapper for vim
Plug 'brooth/far.vim' "asynchronous search and replace operations on a set of files 
Plug 'Shougo/context_filetype.vim' "add context filype feature
"Plug 'Shougo/neopairs.vim' "insert parenthses pairs automatically
Plug 'Shougo/echodoc.vim' "print completed documention
Plug 'Shougo/neoinclude.vim' "complete candidates from included files and path
Plug 'Konfekt/FastFold' "Speed up updating folds when using auto completions plugin
Plug 'ervandew/supertab'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'sudo ./install --all' }
Plug 'junegunn/fzf.vim'
Plug 'sirver/UltiSnips'
Plug 'honza/vim-snippets'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'morhetz/gruvbox'
Plug 'jiangmiao/auto-pairs'
Plug 'wincent/loupe'
Plug 'vim-airline/vim-airline'
Plug 'edkolev/tmuxline.vim'
"Plug 'wincent/terminus' "better integration of terminal (curis behavior, better mouse s upport, focus reporting)
" PHP Plugins
"Plug 'pbogut/deoplete-padawan' "sources for php
Plug 'lvht/phpcd.vim' , { 'for': 'php' , 'do': 'composer install' }"Php Omnifunc
Plug 'phpactor/phpactor' , {'do': 'composer install', 'for': 'php'}
"Plug 'kristijanhusak/deoplete-phpactor' , {'for': 'php'}
Plug 'noahfrederick/vim-composer'
"synatx for ejs
Plug 'nikvdp/ejs-syntax', {'for': 'ejs'}
call plug#end()

"General settings
"set wildmenu "Show a bar that you can use to expand searches with tabs set by
"defaut on neovim
let g:html_indent_script1 = "inc"  "better indentation for hml using javascript
let g:html_indent_style1 = "inc" "better indentation for html using css
set path+=**  "allow to search a file with :find command on all subdirectories
set nu "Add lines number
set relativenumber
set cursorline
set laststatus=2
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%0.3b]\ [HEX=\%02.2B]\ [POS=%04v]\ [%p%%]\ [LEN=%L]
syntax on "Add syntax
nnoremap <C-n> :noh<CR>
set autoindent
set encoding=utf-8
set nocompatible
set expandtab "use always tab
set termguicolors
set formatoptions+=j "remove comment leader when joining comments
set formatoptions+=n "smart auto-indenting inside numbered of lists
set hidden "hide unsaved buffers when changing buffer
set nojoinspaces
set directory^=$HOME/tmp/.nvim/swp//
set showcmd
set foldmethod=indent
set foldignore=
colorscheme gruvbox "use the  gruvbox colortheme

"managing normal mode movements 
set whichwrap=b,h,l,s,<,>,[,],~
"managing visual mode
set virtualedit=block "allow the cursor to move where there is not text
"managing buffers and tab
set switchbuf=usetab "try to reuse windows/tabs when opening a file in a buffer
set tabstop=2 "spaces per tab

"liste chars (changing appearance of space, tab etc)
set list "show whitespace
set listchars=nbsp:☠
set listchars+=tab:>-
set listchars+=extends:»
set listchars+=precedes:«
set listchars+=trail:•
let &showbreak='+++'

"neovim messages management
set shortmess+=A "ignore swapfiles messages

"tab length and behavior managing
set shiftround "always indent by multiple of shiftwidth
set shiftwidth=2 "space per tab
set smarttab "tab/bs indent/detent coressponding to shiftwidth and softtabstop
set softtabstop=-1 "use shiftwidth for tabs/bse

"scrolling around corner 
set scrolloff=3 "start scrolling 3 lines before edge of viewport
set sidescrolloff=3

" Gruvbox settings
set background=dark    " Setting dark mode
"let $NVIM_TUI_ENABLE_TRUE_COLOR=1"

" Deoplete settings
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni#functions = {}
let g:deoplete#omni#functions.javascript = [
      \ 'tern#Complete',
      \ 'jspc#omni'
      \]
"clang complete settings
let g:clang_library_path='/usr/lib/libclang.so.5'

"NERDtree settings
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeChDirMode = 2
let g:airline#extensions#gutentags#enabled = 1

" FZF settings
"nnoremap <C-p> :Hist<CR>
let g:fzf_buffers_jump = 1

"tern settings
"call deoplete#enable_logging('DEBUG', 'deoplete.log')
set completeopt=longest,menuone,preview
let g:deoplete#sources = {}
let g:deoplete#sources['javascript.jsx'] = ['file', 'ultisnips', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent']
let g:deoplete#sources#ternjs#tern_bin = '/usr/bin/tern'
"let g:deoplete#sources#ternjs#timeout = 1
"let g:deoplete#sources#ternjs#depths = 1
let g:deoplete#sources#ternjs#docs = 1
"let g:deoplete#sources#ternjs#filter = 0
let g:deoplete#sources#ternjs#case_insensitive = 1
let g:deoplete#sources#ternjs#omit_object_prototype = 0
let g:deoplete#sources#ternjs#include_keywords = 1
"let g:tern_request_timeout = 1
"let g:tern_request_timeout = 6000
let g:deoplete#sources#tss#javascript_support = 1
let g:tsuquyomi_javascript_support = 1
let g:tsuquyomi_auto_open = 1
"let g:tsuquyomi_disable_quickfix = 1

"Neomake settings
let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_javascript_eslint_exe = $PWD .'/node_modules/.bin/eslint'
"Make Deoplete work nicely with UltiSnips
let g:SuperTabDefaultCompletionType = '<C-n><C-p>'

" better key bindings for UltiSnipsExpandTrigger                                                                 
let g:UltiSnipsExpandTrigger = "<C-s>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

"PHPACTOR settings and keybinding
" Include use statement
nmap <Leader>pu :call phpactor#UseAdd()<CR>
" Invoke the context menu
nmap <Leader>pmm :call phpactor#ContextMenu()<CR>
" Invoke the navigation menu
nmap <Leader>pnn :call phpactor#Navigate()<CR>
" Goto definition of class or class member under the cursor
nmap <Leader>po :call phpactor#GotoDefinition()<CR>
" Transform the classes in the current file
nmap <Leader>ptt :call phpactor#Transform()<CR>
" Generate a new class (replacing the current file)
nmap <Leader>pcc :call phpactor#ClassNew()<CR>
" Extract expression (normal mode)
nmap <silent><Leader>pee :call phpactor#ExtractExpression(v:false)<CR>
" Extract expression from selection
vmap <silent><Leader>pee :<C-U>call phpactor#ExtractExpression(v:true)<CR>
" Extract method from selection
vmap <silent><Leader>pem :<C-U>call phpactor#ExtractMethod()<CR>

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
inoremap <C-h> <C-\><C-N><C-w>h
inoremap <C-j> <C-\><C-N><C-w>j
inoremap <C-k> <C-\><C-N><C-w>k
inoremap <C-l> <C-\><C-N><C-w>l

"terminal mapping
tnoremap <C-h> <C-\><C-N><C-w>h
tnoremap <C-j> <C-\><C-N><C-w>j
tnoremap <C-k> <C-\><C-N><C-w>k
tnoremap <C-l> <C-\><C-N><C-w>l
tnoremap <Esc> <C-\><C-n> 

"leader remaping
"Allows to save a file that need sudo permissions
nmap <Leader>w :w !sudo tee %<CR>
nmap <Leader>ne :NERDTreeToggle<CR>

"auto commands
set autoread "detetec when the files have been change outside of vim or by another buffer of this file
au FocusGained * :checktime "when vim can back the focus apply checktime which load back the file it's modified
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif "Auto close the deoplete preview window when completion is done
"au BufNewFile,BufRead *.ejs set filetype=html "enable ejs extensions as html files
