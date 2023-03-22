# aliases
alias ls='ls -FG'
alias ll='ls -lhG'
alias pyserve="python3 -m http.server"
alias relaunchpad="defaults write com.apple.dock ResetLaunchPad -bool true; killall Dock"

# iTerm2 shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Add Visual Studio Code (code)
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

# n - Node.js version manager
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
