source ~/.profile
# aliases
alias ls="ls -FG"
alias ll="ls -lh"
alias pyserve="python3 -m http.server"
alias relaunchpad="defaults write com.apple.dock ResetLaunchPad -bool true; killall Dock"

# git bash completion
if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
  . /usr/local/etc/bash_completion.d/git-completion.bash
fi

# git bash prompt
if [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
  source /usr/local/etc/bash_completion.d/git-prompt.sh
fi

# 2-line custom prompt:
# - [current working directory] (git branch if applicable)
# - username@hostname $
export PS1="$(tput bold)[\w]\$(tput sgr0)\$(__git_ps1)\n\u@\h $ "

# iTerm2 shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash";

# n - Node.js version manager
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
