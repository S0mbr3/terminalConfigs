{ pkgs }:
with pkgs;
[
  # home-manager
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
  sops
  # openssh
  nodePackages.prettier
  emacsLejiWithPackages
  jansson
  emacsPackages.cask
  telegram-desktop
  # emacsPackages.treesit-grammars.with-all-grammars

  # tree-sitter-grammars.tree-sitter-php
]
