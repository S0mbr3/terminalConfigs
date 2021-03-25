"init plugins loading"
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
			  \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin()
Plug 'neovim/nvim-lspconfig'
Plug 'wellle/tmux-complete.vim' "sources for tmux panes
Plug 'brooth/far.vim' "asynchronous search and replace operations on a set of files 
Plug 'Shougo/echodoc.vim' "print completed documention
Plug 'Shougo/neoinclude.vim' "complete candidates from included files and path
Plug 'Shougo/context_filetype.vim' "add context filype feature
Plug 'tpope/vim-surround' "Plugging that allow to surround text
Plug 'tpope/vim-fugitive' "Git wrapper for vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'sudo ./install --all' }
Plug 'junegunn/fzf.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'wincent/loupe'
Plug 'vim-airline/vim-airline'
Plug 'edkolev/tmuxline.vim'
Plug 'morhetz/gruvbox'
call plug#end()


"loading my configurations into init.vim file
 for f in glob('~/.config/nvim/configs/*.vim', 0, 1)
	      execute 'source' f
endfor

