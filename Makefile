help:
	@echo "=> Targets:"
	@echo "vim, mutt, zsh, git, tmux, irssi, newsbeuter, mpd, xresources, \
	xinitrc, frankenwm, vimperator, conky-desktop, conky-netbook"
	@echo "=> Groups:"
	@echo "dev, cli, gui, all"

LN=ln -s

vim:
	$(LN) $(shell pwd)/.vimrc ~/.vimrc
	$(LN) $(shell pwd)/.vim ~/.vim

mutt:
	$(LN) $(shell pwd)/mutt/.muttrc ~/.muttrc
	$(LN) $(shell pwd)/mutt/.offlineimaprc ~/.offlineimaprc

zsh:
	$(LN) $(shell pwd)/zsh/.zshrc ~/.zshrc

git:
	$(LN) $(shell pwd)/.gitconfig ~/.gitconfig

tmux:
	$(LN) $(shell pwd)/.tmux.conf ~/.tmux.conf

irssi:
	$(LN) $(shell pwd)/.irrsi ~/.irssi

newsbeuter:
	$(LN) $(shell pwd)/.newsbeuter ~/.newsbeuter

mpd:
	$(LN) $(shell pwd)/mpd/.mpdconf ~/.mpdconf
	$(LN) $(shell pwd)/mpd/.ncmpcpp ~/.ncmpcpp

xresources:
	$(LN) $(shell pwd)/.Xresources ~/.Xresources

xinitrc:
	$(LN) $(shell pwd)/.xinitrc ~/.xinitrc

frankenwm:
	$(LN) $(shell pwd)/.frankenwm.config.h ~/build/frankenwm/config.h

vimperator:
	$(LN) $(shell pwd)/.vimperator ~/.vimperator
	$(LN) $(shell pwd)/.vimperatorrc ~/.vimperatorrc

conky-desktop:
	$(LN) $(shell pwd)/.conkyrc_desktop ~/.conkyrc

conky-netbook:
	$(LN) $(shell pwd)/.conkyrc_netbook ~/.conkyrc

dev: vim mutt zsh git tmux

cli: vim mutt zsh git tmux irssi newsbeuter mpd xresources

gui: xinitrc frankenwm vimperator conky-desktop

all: cli gui

