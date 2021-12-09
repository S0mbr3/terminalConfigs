vim.o.clipboard=vim.o.clipboard.."unnamedplus"

--vim.o.wildmenu --Show a bar that you can use to expand searches with tabs set by
--defaut on neovim
--let g:html_indent_script1 = 'inc'  --better indentation for hml using javascript
--let g:html_indent_style1 = 'inc' --better indentation for html using css
vim.o.path=vim.o.path .. "**"  --allow to search a file with :find command on all subdirectories
vim.o.nu=true --Add lines number
--vim.o.relativenumber
vim.o.cursorline=true
vim.o.laststatus=2
vim.o.statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%0.3b]\ [HEX=\%02.2B]\ [POS=%04v]\ [%p%%]\ [LEN=%L]
vim.o.syntax=true --Add syntax
vim.o.encoding="utf-8"
vim.o.expandtab=true --use always tab
vim.o.termguicolors=true
vim.o.formatoptions=vim.o.formatoptions .. "j" --remove comment leader when joining comments
vim.o.formatoptions=vim.o.formatoptions .. "n" --smart auto-indenting inside numbered of lists
vim.o.hidden=true --hide unsaved buffers when changing buffer
vim.o.nojoinspaces=true
--vim.o.directory^="$HOME/tmp/.nvim/swp//"
vim.o.showcmd=true
vim.o.foldmethod="indent"
vim.o.foldignore=
vim.o.completeopt=='menuone,noselect' --complete options
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy'] --how completion fill the menu


colorscheme gruvbox --use the  gruvbox colortheme

--managing normal mode movements 
vim.o.whichwrap="b,h,l,s,<,>,[,],~"
--managing visual mode
vim.o.virtualedit="block" --allow the cursor to move where there is not text
--managing buffers and tab
vim.o.switchbuf="usetab" --try to reuse windows/tabs when opening a file in a buffer
vim.o.tabstop=2 --spaces per tab

--liste chars (changing appearance of space, tab etc)
vim.o.list=true --show whitespace
vim.o.listchars="nbsp:☠"
vim.o.listchars=vim.o.listchars .. "tab:>-"
vim.o.listchars=vim.o.listchars .. "extends:»"
vim.o.listchars=vim.o.listchars .. "precedes:«"
vim.o.listchars=vim.o.listchars .. "trail:•"
let &showbreak='+++'

--neovim messages management
vim.o.shortmess="+=A" --ignore swapfiles messages

--tab length and behavior managing
vim.o.shiftround=true --always indent by multiple of shiftwidth
vim.o.shiftwidth=2 --space per tab
vim.o.softtabstop=-1 --use shiftwidth for tabs/bse

--scrolling around corner
vim.o.scrolloff=3 --start scrolling 3 lines before edge of viewport
vim.o.sidescrolloff=3
