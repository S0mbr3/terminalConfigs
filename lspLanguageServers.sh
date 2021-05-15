sudo npm i -g pyright #python language server
sudo pip install -U jedi-language-server
sudo snap install ccls --classic
sudo npm i -g bash-language-server
sudo pip install cmake-language-server
sudo npm install -g dockerfile-language-server-nodejs
yarn global add diagnostic-languageserver
sudo npm install -g vscode-html-languageserver-bin
#curl -fsSL https://deno.land/x/install/install.sh | sh
sudo npm install -g typescript-language-server
(git clone https://github.com/sumneko/lua-language-server $HOME/builds/lua-language-server \
  && cd $HOME/builds/lua-language-server && git submodule update --init --recursive \
  cd 3rd/luamake && compile/install.sh && cd ../.. && ./3rd/luamake/luamake rebuild)
