# set PATH so it includes go binaries
if [ -d "/usr/local/go/bin" ] ; then
    PATH="/usr/local/go/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$PATH:$HOME/.cargo/bin"
fi

eval "$(/opt/homebrew/bin/brew shellenv)"
