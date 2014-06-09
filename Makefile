help:
	echo -e "\nTargets: vim, mutt, zsh, git, tmux, irssi, i3, devel, all\n"

LN=ln -s

vim:
	$(LN) .vimrc ~/.vimrc
	$(LN) .vim ~/.vim

mutt:
	$(LN) mutt/.muttrc ~/.muttrc

zsh:
	$(LN) zsh/.zshrc ~/.zshrc

git:
	$(LN) .gitconfig ~/.gitconfig

tmux:
	$(LN) .tmux.conf ~/.tmux.conf

irssi:
	$(LN) .irrsi ~/.irssi

i3:
	$(LN) .i3 ~/.i3

devel: vim mutt zsh tmux git

all: vim mutt zsh git tmux irssi i3

