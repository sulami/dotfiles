#!/bin/bash

defaults write -g ApplePressAndHoldEnabled -bool false
defaults write -g InitialKeyRepeat -int 25
defaults write -g KeyRepeat -int 2
defaults write -g NSAutomaticWindowAnimationsEnabled -bool false
defaults write -g AppleShowAllExtensions -bool true
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
defaults write -g PMPrintingExpandedStateForPrint -bool true
defaults write com.apple.dashboard mcx-disabled -boolean YES && killall Dock
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.dock mru-spaces -bool false
defaults write com.apple.dock workspaces-auto-swoosh -bool NO
defaults write com.apple.finder NewWindowTarget -string "PfLo" && \
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
defaults write com.apple.menuextra.battery ShowPercent -string "YES"
defaults write com.apple.menuextra.battery ShowTime -string "NO"
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true
defaults write com.apple.TextEdit RichText -int 0
chflags nohidden ~/Library

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install zsh tmux vim python3 wget watch jq
brew tap caskroom/fonts
brew cask install iterm2 font-inconsolata bettertouchtool firefox dropbox skype steam vlc telegram-desktop mumble libreoffice dwihn0r-keepassx google-chrome

git clone ssh://pi@peerwire.dtdns.net:/srv/git/dotfiles.git ~/dotfiles
cd ~/dotfiles
make zsh vim tmux git
cd ~

chsh -s /bin/zsh $(whoami)

pip3 install bpython pytest virtualenvwrapper django
