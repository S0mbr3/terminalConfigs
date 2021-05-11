
#install autosuggestions and syntax-highlighting plugins for zsh
sudo git clone https://github.com/zsh-users/zsh-autosuggestions.git $ZSH/plugins/zsh-autosuggestions
sudo git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH/plugins/zsh-syntax-highlighting


#installing powerlevel10k for zsh
sudo git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $ZSH/themes/powerlevel10k
p10k configure

#installing fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
sudo ~/.fzf/install

#installing Z plugin for zsh
sudo git clone https://github.com/agkozak/zsh-z $ZSH/plugins/zsh-z

#downloading and installing lsd (colors for ls commands)
(cd $HOME/builds && curl -OfsSL https://github.com/Peltoche/lsd/releases/download/0.20.1/lsd-musl_0.20.1_amd64.deb)
sudo dpkg -i $HOME/builds/lsd-musl_0.20.1_amd64.deb # adapt version number and architecture

#reloading shell
source ~/.zshrc


#repos of zsh plugins
#git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions 
#git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting
