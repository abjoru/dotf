#!/bin/bash
#
# osx-setup.sh

#################
# General UI/UX #
#################

# Disable press-and-hold for keys in favor of key repeat
defaults write -g ApplePressAndHoldEnabled -bool false

# Use AirDrop over every interface
defaults write com.apple.NetworkBrowser BrowseAllInterfaces 1

# Always open everything in Finder's list view
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Run screensaver from bottom-left corner
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0

################
# Safari setup #
################

# Set the homepage
defaults write com.apple.Safari HomePage -string 'file:///Users/abjoru/.cache/dotf/homepage.html'
# Hide bookmark bar
defaults write com.apple.Safari ShowFavoritesBar -bool false
# WebDev Stuff
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

#########
# ITerm #
#########

# Don't display the annoying prompt when quitting
defaults write com.googlecode.iterm2 PromptOnQuit -bool false
