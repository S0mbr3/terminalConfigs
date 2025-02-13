{ pkgs }:
with pkgs;
[
  ranger
  neovim
  tmux
  fzf
  cowsay
  fortune
  toilet
  lolcat
  fd
  jq
  ripgrep
  lazygit
  lsd
  nodenv
  pipx
  stow
  kitty
  wezterm
  chafa
  gnupg
  atuin
  pyenv
  zsh-history-substring-search
  luarocks
  luajit
  emacsLejiWithPackages
  jansson
  emacsPackages.cask
  # emacsPackages.treesit-grammars.with-all-grammars

  # tree-sitter-grammars.tree-sitter-php
]
