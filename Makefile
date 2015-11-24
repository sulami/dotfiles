help:
	@echo "=> Groups:"
	@echo "dev, cli, gui, all"

LN=ln -sf

vim:
	$(LN) ${HOME}/dotfiles/.vimrc ${HOME}/
	$(LN) ${HOME}/dotfiles/.vim ${HOME}/

mutt:
	$(LN) ${HOME}/dotfiles/mutt/.muttrc ${HOME}/
	$(LN) ${HOME}/dotfiles/mutt/.offlineimaprc ${HOME}/

zsh:
	$(LN) ${HOME}/dotfiles/zsh/.zshrc ${HOME}/

git:
	$(LN) ${HOME}/dotfiles/.gitconfig ${HOME}/

tmux:
	$(LN) ${HOME}/dotfiles/.tmux.conf ${HOME}/

top:
	$(LN) ${HOME}/dotfiles/.toprc ${HOME}/

irssi:
	$(LN) ${HOME}/dotfiles/.irrsi ${HOME}/

feed2maildir:
	$(LN) ${HOME}/dotfiles/mutt/.f2mrc ${HOME}/

imgur:
	mkdir -p ${HOME}/.config/imgur-screenshot
	$(LN) ${HOME}/dotfiles/configs/imgur-screenshot/settings.conf \
	${HOME}/.config/imgur-screenshot/

mpd:
	$(LN) ${HOME}/dotfiles/mpd/.mpdconf ${HOME}/
	$(LN) ${HOME}/dotfiles/mpd/.ncmpcpp ${HOME}/

profile:
	$(LN) ${HOME}/dotfiles/.profile ${HOME}/

xinitrc:
	$(LN) ${HOME}/dotfiles/.xinitrc ${HOME}/

xprofile:
	$(LN) ${HOME}/dotfiles/.xprofile ${HOME}/

urxvt:
	mkdir -p ${HOME}/.urxvt/ext
	$(LN) ${HOME}/dotfiles/scripts/font-size ${HOME}/.urxvt/ext/

dev: vim zsh git tmux top profile

cli: dev mutt irssi feed2maildir imgur mpd

gui: xinitrc xprofile urxvt

all: cli gui

