#!/usr/bin/env bash

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