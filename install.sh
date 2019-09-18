#!/bin/bash

# Disable long press for Accents
defaults write -g ApplePressAndHoldEnabled -bool false

# Mimic UNIX terminal key repetition
defaults write -g InitialKeyRepeat -int 25
defaults write -g KeyRepeat -int 2

# Disable window animations
defaults write -g NSAutomaticWindowAnimationsEnabled -bool false

# Show file extensions in Finder
defaults write -g AppleShowAllExtensions -bool true

# Disable automatic text mangling
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
defaults write -g NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write -g NSAutomaticCapitalizationEnabled -bool false
defaults write -g NSAutomaticDashSubstitutionEnabled -bool false
defaults write -g NSAutomaticPeriodSubstitutionEnabled -bool false

# Always show the expanded print dialog
defaults write -g PMPrintingExpandedStateForPrint -bool true
defaults write -g PMPrintingExpandedStateForPrint2 -bool true

# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Scrollbars visible when scrolling
defaults write -g AppleShowScrollBars -string 'WhenScrolling'

# Disable the dashboard
defaults write com.apple.dashboard mcx-disabled -boolean true && killall Dock

# Don't write .DS_store files to network drives
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Disable space reoderding
defaults write com.apple.dock mru-spaces -bool false

# Disable automatic space switching
defaults write com.apple.dock workspaces-auto-swoosh -bool false

# Open ~ by default in Finder
defaults write com.apple.finder NewWindowTarget -string 'PfLo'
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}"

# Don't warn me about changing file extensions
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Search in currend directory by default
defaults write com.apple.finder FXDefaultSearchScope -string 'SCcf'

# Show the full path in the Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Show me the battery percentage, not the time
defaults write com.apple.menuextra.battery ShowPercent -bool true
defaults write com.apple.menuextra.battery ShowTime -string false

# Set TextEdit to plain text by default
defaults write com.apple.TextEdit RichText -int 0

# Ask for authz when locking the screen
defaults write com.apple.screensaver askForPassword -bool true
defaults write com.apple.screensaver askForPasswordDelay -int 5

# Maximize windows on double clicking them
defaults write -g AppleActionOnDoubleClick 'Maximize'

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# Set minimal autohide/show delay for hidden dock
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0.5

# Show only active apps in Dock
defaults write com.apple.dock static-only -bool true

# Disable Dock magnification
defaults write com.apple.dock magnification -bool false

# Minimize windows into their application's icon
defaults write com.apple.dock minimize-to-application -bool true

# Don't show indicator lights for open apps in Dock
defaults write com.apple.dock show-process-indicators -bool false

# Disable delay when rendering web pages in Safari
defaults write com.apple.Safari WebKitInitialTimedLayoutDelay 0.25

# Enable the Develop menu and the Web Inspector in Safari
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled -bool true
defaults write -g WebKitDeveloperExtras -bool true

# Defancy Emacs (also faster here than inside Emacs)
defaults write org.gnu.Emacs HideDocumentIcon -bool true
defaults write org.gnu.Emacs ToolBar -string no

# Add a context menu item for showing the Web Inspector in web views
defaults write -g WebKitDeveloperExtras -bool true

# Disable autocorrect in Safari
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false

# Don’t display prompt when quitting iTerm
defaults write com.googlecode.iterm2 PromptOnQuit -bool false

# Show all processes in Activity Monitor
defaults write com.apple.ActivityMonitor ShowCategory -int 0

# Don't hide ~/Library
chflags nohidden ~/Library

# Set dark theme
sudo defaults write /Library/Preferences/.GlobalPreferences AppleInterfaceTheme Dark

# Install homebrew
which brew || /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew bundle

chsh -s /bin/zsh $(whoami)i
