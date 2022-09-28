#!/bin/zsh
#Script installing custom linux environement on ubuntu based distros

#: << 'END'

mkdir -p $HOME/.config/nvim $HOME/builds $HOME/.local/bin/fd

#install powerlevel10k fonts to have icons
sudo mkdir /usr/share/fonts/newfonts && sudo cp powerlevel10kPolices/* /usr/share/fonts/newfonts
fc-cache -f -v

sudo $1 $2 git
sudo $1 $2 cmake
sudo $1 $2 rlwrap
sudo $1 $2 tree
sudo $1 $2 stow

#configuring git global name and email
echo "Global configuration for git user.name and user.email"
read -p "Enter your user.name: " name
read -p "Enter your user.email: " email
git config --global user.name $name
git config --global user.email $email

# dependencies to install neovim from source
if [ $1 == "apt-get" ]; then
	echo "preparing for ubuntu based distro\n"
	sudo apt-get update && sudo apt-get upgrade
	sudo apt-get install libtool
	sudo apt-get install libtool-bin
	sudo apt-get install gettext
	sudo apt-get install libevent-2.1-7 libevent-core-2.1-7 libevent-extra-2.1-7 libevent-dev
	sudo apt-get install libncurses5-dev
	sudo apt-get install bison
	sudo apt-get install build-essential
	sudo apt-get install ninja-build snapd pkg-config python ruby-dev nodejs ruby build-essential python3 pip fd-find bat npm xclip zsh
	#downloading and installing lsd (colors for ls commands)
(cd $HOME/builds && curl -OfsSL https://github.com/Peltoche/lsd/releases/download/0.20.1/lsd-musl_0.20.1_amd64.deb)
sudo dpkg -i $HOME/builds/lsd-musl_0.20.1_amd64.deb # adapt version number and architecture

	#downloading neovim
	git clone https://github.com/neovim/neovim.git $HOME/builds/neovim
	#building neovim
	make -C $HOME/builds/neovim CMAKE_BUILD_TYPE=RelWithDebInfo
	sudo make -C $HOME/builds/neovim install

#downloading and building tmux from source
git clone https://github.com/tmux/tmux.git $HOME/builds/tmux
(cd $HOME/builds/tmux && sh autogen.sh && ./configure && make)

elif [ $1 == "pacman" ]; then
	echo "preparing for arch based distro\n"
	sudo pacman -S ninja python python2 python-pip nodejs npm ruby fd zsh xclip bat tmux curl lsd dpkg z ripgrep lf mdp
	git clone https://aur.archlinux.org/snapd.git $HOME/builds/snapd
	(cd $HOME/builds/snapd && makepkg -si)
	sudo systemctl enable --now snapd.socket
	sudo ln -s /var/lib/snapd/snap /snap
fi


# downloand and install cht.sh
curl https://cht.sh/:cht.sh | sudo tee /usr/local/bin/cht.sh
sudo chmod +x /usr/local/bin/cht.sh


#install Usefull tools 
#python neovim module
sudo npm install -g browser-sync
sudo npm install -g yarn
yarn global add tree-sitter-cli
yarn global add tree-sitter
python3 -m pip install --user --upgrade pynvim
sudo gem install neovim
sudo npm install -g neovim

#install Oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

#making zsh default shell
sudo chsh -s $(which zsh)

# preparing and create symlinks
rm $HOME/.zshrc $HOME/.config/nvim/init.vim $HOME/.tmux.conf
ln -s $(pwd)/.zshrc $HOME/ && ln -s $(which fdfind) $HOME/.local/bin/fd && ln -s $(pwd)/.tmux.conf $HOME/
ln -s $(pwd)/init.vim $HOME/.config/nvim && ln -s $(pwd)/configs $HOME/.config/nvim





#END

