#Script installing custom linux environement on ubuntu based distros

#: << 'END'
sudo apt-get update && sudo apt-get upgrade

mkdir -p $HOME/.config/nvim $HOME/builds $HOME/.local/bin/fd

#install powerlevel10k fonts to have icons
sudo mkdir /usr/share/fonts/truetype/newfonts && sudo mv powerlevel10kPolices/* /usr/share/fonts/truetype/newfonts
fc-cache -f -v

# dependencies to install neovim from source
sudo apt-get install cmake
sudo apt-get install libtool
sudo apt-get install libtool-bin
sudo apt-get install gettext
sudo apt-get install libevent-2.1-7 libevent-core-2.1-7 libevent-extra-2.1-7 libevent-dev
sudo apt-get install libncurses5-dev
sudo apt-get install bison
sudo apt-get install rlwrap

#configuring git global name and email
echo "Global configuration for git user.name and user.email"
read -p "Enter your user.name: " name
read -p "Enter your user.email: " email
git config --global user.name $name
git config --global user.email $email

# downloand and install cht.sh
curl https://cht.sh/:cht.sh | sudo tee /usr/local/bin/cht.sh
sudo chmod +x /usr/local/bin/cht.sh

#downloading neovim
git clone https://github.com/neovim/neovim.git $HOME/builds/neovim
#building neovim
make -C $HOME/builds/neovim CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make -C $HOME/builds/neovim install

#downloading and building tmux from source
git clone https://github.com/tmux/tmux.git $HOME/builds/tmux
(cd $HOME/builds/tmux && sh autogen.sh && ./configure && make)

#install Usefull tools 
sudo apt-get install pkg-config python nodejs ruby build-essential python3 pip fd-find bat npm xclip zsh
#python neovim module
python3 -m pip install --user --upgrade pynvim

#making zsh default shell
sudo chsh -s $(which zsh)

# preparing and create symlinks
rm $HOME/.zshrc $HOME/.config/nvim/init.vim $HOME/.tmux.conf
ln -s $(pwd)/.zshrc $HOME/ && ln -s $(which fdfind) $HOME/.local/bin/fd && ln -s $(pwd)/.tmux.conf $HOME/
ln -s $(pwd)/init.vim $HOME/.config/nvim && ln -s $(pwd)/configs $HOME/.config/nvim

#install Oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"




#END

