if which nvim > /dev/null 2>&1; then
    # Enable neovim if it is installed.
    export EDITOR=nvim
    export VISUAL=nvim
else
    export EDITOR=vim
    export VISUAL=vim
fi
export LC_ALL=en_US.UTF-8
export BROWSER=firefox
export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig
export GOPATH=$HOME/build/go
export GOMAXPROCS=8
export GITSERVER=pi@peerwire.dtdns.net
export GITURL=ssh://${GITSERVER}/srv/git
export XDG_CONFIG_HOME=$HOME
