{ pkgs }:
with pkgs;
[
  # home-manager
  #ranger
  neovim
  yazi
  tmux
  fzf
  cowsay
  fortune
  toilet
  lolcat
  fd
  jq
  yq
  ripgrep
  lazygit
  lsd
  nodenv
  pipx
  #rustup
  rust-bin.nightly.latest.default
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
  age
  # openssh
  nodePackages.prettier
  emacsLejiWithPackages
  jansson
  emacsPackages.cask
  telegram-desktop
  gawk
  #floorp
  tor
  yt-dlp
  aria2
  mpv
  aider-chat
  texliveFull
  #direnv
  # emacsPackages.treesit-grammars.with-all-grammars

  # tree-sitter-grammars.tree-sitter-php
]
