alias ls='ls -FG'
alias ll='ls -lhG'
export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
export PATH="/usr/local/smlnj/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/.emacs.d/bin:$PATH"
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export PATH="/usr/local/opt/avr-gcc@8/bin:$PATH"
