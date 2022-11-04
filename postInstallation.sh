#!/bin/bash

#install autosuggestions and syntax-highlighting plugins for zsh
sudo git clone https://github.com/zsh-users/zsh-autosuggestions.git $ZSH/plugins/zsh-autosuggestions
sudo git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH/plugins/zsh-syntax-highlighting


#installing powerlevel10k for zsh
#sudo git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH/themes/powerlevel10k
#source ~/.oh-my-zsh/themes/powerlevel10k/powerlevel10k.zsh-theme >> ~/.zshrc
#p10k configure

#installing fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
sudo ~/.fzf/install

#installing Z plugin for zsh
sudo git clone https://github.com/agkozak/zsh-z $ZSH/plugins/zsh-z



#install lsp servers
source lspLanguageServers.sh
#reloading shell
source ~/.zshrc


#repos of zsh plugins
#git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions 
#git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting
