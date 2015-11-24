help:
	@echo "=> Groups:"
	@echo "dev, cli, gui, all"

LN=ln -s

vim:
	$(LN) $(shell pwd)/.nvimrc ${HOME}/
	$(LN) $(shell pwd)/.nvim ${HOME}/

mutt:
	$(LN) $(shell pwd)/mutt/.muttrc ${HOME}/
	$(LN) $(shell pwd)/mutt/.offlineimaprc ${HOME}/

zsh:
	$(LN) $(shell pwd)/zsh/.zshrc ${HOME}/

git:
	$(LN) $(shell pwd)/.gitconfig ${HOME}/

tmux:
	$(LN) $(shell pwd)/.tmux.conf ${HOME}/

top:
	$(LN) $(shell pwd)/.toprc ${HOME}/

irssi:
	$(LN) $(shell pwd)/.irrsi ${HOME}/

feed2maildir:
	$(LN) $(shell pwd)/mutt/.f2mrc ${HOME}/

imgur:
	mkdir -p ${HOME}/.config/imgur-screenshot
	$(LN) $(shell pwd)/configs/imgur-screenshot/settings.conf \
	${HOME}/.config/imgur-screenshot/

mpd:
	$(LN) $(shell pwd)/mpd/.mpdconf ${HOME}/
	$(LN) $(shell pwd)/mpd/.ncmpcpp ${HOME}/

profile:
	$(LN) $(shell pwd)/.profile ${HOME}/

xinitrc:
	$(LN) $(shell pwd)/.xinitrc ${HOME}/

xprofile:
	$(LN) $(shell pwd)/.xprofile ${HOME}/

urxvt:
	mkdir -p ${HOME}/.urxvt/ext
	$(LN) $(shell pwd)/scripts/font-size ${HOME}/

dev: vim zsh git tmux top profile

cli: dev mutt irssi feed2maildir imgur mpd

gui: xinitrc xprofile urxvt

all: cli gui

