help:
	@echo "=> Targets:"
	@echo "nvim, mutt, zsh, git, tmux, irssi, feed2maildir, imgur, mpd, \
	xresources, xinitrc, vimperator"
	@echo "=> Groups:"
	@echo "dev, cli, gui, all"

LN=ln -s

nvim:
	$(LN) $(shell pwd)/.nvimrc ~/.nvimrc
	$(LN) $(shell pwd)/.nvim ~/.nvim

mutt:
	$(LN) $(shell pwd)/mutt/.muttrc ~/.muttrc
	$(LN) $(shell pwd)/mutt/.offlineimaprc ~/.offlineimaprc

zsh:
	$(LN) $(shell pwd)/zsh/.zshrc ~/.zshrc.local

git:
	$(LN) $(shell pwd)/.gitconfig ~/.gitconfig

tmux:
	$(LN) $(shell pwd)/.tmux.conf ~/.tmux.conf

irssi:
	$(LN) $(shell pwd)/.irrsi ~/.irssi

feed2maildir:
	$(LN) $(shell pwd)/mutt/.f2mrc ~/.f2mrc

imgur:
	mkdir -p ~/.config/imgur-screenshot
	$(LN) $(shell pwd)/configs/imgur-screenshot-settings.conf \
	~/.config/imgur-screenshot/settings.conf

mpd:
	$(LN) $(shell pwd)/mpd/.mpdconf ~/.mpdconf
	$(LN) $(shell pwd)/mpd/.ncmpcpp ~/.ncmpcpp

xresources:
	$(LN) $(shell pwd)/Xresources/.Xresources ~/.Xresources

xinitrc:
	$(LN) $(shell pwd)/.xinitrc ~/.xinitrc

vimperator:
	$(LN) $(shell pwd)/firefox/.vimperator ~/.vimperator
	$(LN) $(shell pwd)/firefox/.vimperatorrc ~/.vimperatorrc

dev: nvim mutt zsh git tmux

cli: nvim mutt zsh git tmux irssi feed2maildir mpd xresources imgur

gui: xinitrc vimperator

all: cli gui

