set clipboard+=unnamedplus

"set wildmenu "Show a bar that you can use to expand searches with tabs set by
"defaut on neovim
let g:html_indent_script1 = 'inc'  "better indentation for hml using javascript
let g:html_indent_style1 = 'inc' "better indentation for html using css
set path+=**  "allow to search a file with :find command on all subdirectories
set nu "Add lines number
"set relativenumber
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
set completeopt=menuone,noselect "complete options
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy'] "how completion fill the menu


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
