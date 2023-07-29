vim.o.clipboard=vim.o.clipboard.."unnamedplus"

--vim.o.wildmenu --Show a bar that you can use to expand searches with tabs set by
--defaut on neovim
--vim.cmd('/home/oxhart/.nvm/versions/node/v16.10.0/bin/neovim-node-host')
--vim.cmd([[let $PATH = '/home/oxhart/.nodenv/versions/16.17.1/bin:' . $PATH]])
--vim.cmd([[let $PATH = '/home/oxhart/.nvm/versions/node/16.10.0/bin:' . $PATH]])
vim.cmd([[let $PATH = '/home/oxhart/.nodenv/versions/16.10.0/bin:' . $PATH]])
vim.g.html_indent_script1 = 'inc'  --better indentation for hml using javascript
vim.g.html_indent_style1 = 'inc' --better indentation for html using css
vim.o.path=vim.o.path .. "**"  --allow to search a file with :find command on all subdirectories
vim.o.nu=true --Add lines number
vim.o.relativenumber=true
vim.g.suda_smart_edit = 1
--vim.o.relativenumber
vim.o.cursorline=true
vim.o.laststatus=2
vim.o.insearch=false --show searching as typing
vim.o.hlsearch=false -- highliting or no the searches
--vim.o.statusline=[[ %F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%0.3b]\ [HEX=\%02.2B]\ [POS=%04v]\ [%p%%]\ [LEN=%L] ]]
--vim.cmd([[syntax on ]]) --Add syntax
--vim.o.autoindent=true
vim.o.smartindent=true
vim.o.encoding="utf-8"
vim.o.expandtab=true --use always tab
vim.o.termguicolors=true
vim.o.formatoptions=vim.o.formatoptions .. "j" --remove comment leader when joining comments
vim.o.formatoptions=vim.o.formatoptions .. "n" --smart auto-indenting inside numbered of lists
vim.o.hidden=true --hide unsaved buffers when changing buffer
vim.cmd([[ set nojoinspaces ]])
vim.cmd([[ set directory^="$HOME/tmp/.nvim/swp//" ]])
vim.o.showcmd=true
--vim.o.foldmethod="indent"
--vim.cmd([[ set foldignore=]])
vim.o.completeopt="menu,menuone,noinsert" --complete options
--vim.cmd([[ let g:completion_matching_strategy_list=["exact", "substring", "fuzzy"] ]])--how completion fill the menu


--vim.g.material_style = "palenight"
-- vim.cmd([[
-- colorscheme material
-- ]]) --use the  gruvbox colortheme

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
vim.cmd([[
set listchars+=tab:>-
set listchars+=extends:»
set listchars+=precedes:«
set listchars+=trail:•
]])
-- enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

vim.cmd([[let &showbreak='+++']])
--neovim messages management
vim.cmd([[ set shortmess="+=A" ]])--ignore swapfiles messages

--tab length and behavior managing
vim.o.shiftround=true --always indent by multiple of shiftwidth
vim.o.shiftwidth=2 --space per tab
vim.o.smarttab=true
vim.o.softtabstop=-1 --use shiftwidth for tabs/bse

--scrolling around corner
vim.o.scrolloff=3 --start scrolling 3 lines before edge of viewport
vim.o.sidescrolloff=3

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
