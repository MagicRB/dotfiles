#!/usr/bin/env bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

## Enable color on grep
alias grep='grep --color=auto'

## Replace ls and cat with exa and bat respectively
alias ls='exa'
alias cat='bat'

## Add .local/bin to path
#export PATH="${HOME}/.local/bin:${PATH}"

if [[ -d "${HOME}/.cargo/bin" ]]; then
    export PATH="${HOME}/.cargo/bin:${PATH}"
fi

if [[ -d "${HOME}/.yarn/bin" ]]; then
    export PATH="${HOME}/.yarn/bin:${PATH}"
fi

vterm_printf() {
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "\u@\[\e[37m\]\h\[\e[m\]:\[\e[32m\]\w\[\e[m\]\[\e[31m\]\\$\[\e[m\] "
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

if [[ ! -z "${INSIDE_EMACS+x}" ]] && [[ ! -z "${SSH_CONNECTION+x}" ]]
then
    alias E="emacsclient-remote --sudo"
    alias e="emacsclient-remote"
elif [[ ! -z "${INSIDE_EMACS}" ]]
then
    EMACSCLIENT="`command -v -- emacsclient >/dev/null 2>&1 && printf emacsclient || printf emacsclient-$(uname -m)`"
    echo "${EMACSCLIENT}"
    alias E="SUDO_EDITOR=\"${EMACSCLIENT} -a emacs\" sudoedit"
    alias e="${EMACSCLIENT}"
else
    EMACSCLIENT="`command -v -- emacsclient >/dev/null 2>&1 && printf emacsclient || printf emacsclient-$(uname -m)`"
    echo "${EMACSCLIENT}"
    alias E="SUDO_EDITOR=\"${EMACSCLIENT} -a emacs -t\" sudoedit"
    alias e="${EMACSCLIENT} -t"
fi


[[ ! -z "${INSIDE_EMACS+x}" ]] && alias vim="e"

SSH=$(which ssh)
ssh() {
    if [[ ! -z "${INSIDE_EMACS+x}" ]]
    then
	"${SSH}" \
	    -o "SendEnv INSIDE_EMACS" \
	    -o "StreamLocalBindUnlink yes" \
	    -o "ControlMaster auto" \
	    -o "ControlPath ~/.ssh/controlmasters/%r@%h:%p" \
	    -Nf \
	    -R ~/.ssh/emacs-server:/run/user/1000/emacs/server \
	    $1
	"${SSH}" \
	    -S ~/.ssh/controlmasters/%r@%h:%p \
	    -o 'SendEnv=INSIDE_EMACS' \
	    $@
    else
	"${SSH}" $@
    fi
}

#if [[ -d "/mnt/data3/Programs/bin" ]]; then
#    export PATH="/mnt/data3/Programs/bin:${PATH}"
#fi

## Set prompt
export PS1="\u@\[\e[37m\]\h\[\e[m\]:\[\e[32m\]\w\[\e[m\]\[\e[31m\]\\$\[\e[m\] "

export EDITOR="emacsclient"
export BROWSER="firefox-nightly"

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
