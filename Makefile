help:
	@echo "=> Targets:"
	@echo "vim, nvim, mutt, zsh, git, tmux, top, irssi, feed2maildir, imgur, \
	mpd, xresources, xinitrc"
	@echo "=> Groups:"
	@echo "dev, cli, gui, all"

LN=ln -s

vim:
	$(LN) $(shell pwd)/.nvimrc ~/.vimrc
	$(LN) $(shell pwd)/.nvim ~/.vim

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

top:
	$(LN) $(shell pwd)/.toprc ~/.toprc

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

dev: nvim mutt zsh git tmux top

cli: nvim mutt zsh git tmux irssi feed2maildir mpd xresources imgur top

gui: xinitrc

all: cli gui

