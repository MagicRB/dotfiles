#!/usr/bin/env bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion

## Enable color on grep
alias grep='grep --color=auto'

## Replace ls and cat with exa and bat respectively
alias ls='@exa@/bin/exa'
alias cat='@bat@/bin/bat'

## Add .local/bin to path
#export PATH="${HOME}/.local/bin:${PATH}"

if [[ -d "${HOME}/.cargo/bin" ]]; then
    export PATH="${HOME}/.cargo/bin:${PATH}"
fi

if [[ -d "${HOME}/.yarn/bin" ]]; then
    export PATH="${HOME}/.yarn/bin:${PATH}"
fi

export PS1="\u@\[\e[37m\]\h\[\e[m\]:\[\e[32m\]\w\[\e[m\]\[\e[31m\]\\$\[\e[m\] "

EMACSCLIENT="`command -v -- emacsclient >/dev/null 2>&1 && printf emacsclient || printf emacsclient-$(uname -m)`"
E() {
    if [[ ! -z "${INSIDE_EMACS+x}" ]] && [[ ! -z "${SSH_CONNECTION+x}" ]]
    then
	emacsclient-remote --sudo "$1"
    elif [[ ! -z "${INSIDE_EMACS}" ]]
    then
	"${EMACSCLIENT}" -a emacs "/sudo::/${1}"
    else
	"${EMACSCLIENT}" -t -a emacs "/sudo::/${1}"
    fi
}
e() {
    if [[ ! -z "${INSIDE_EMACS+x}" ]] && [[ ! -z "${SSH_CONNECTION+x}" ]]
    then
	emacsclient-remote "$1"
    elif [[ ! -z "${INSIDE_EMACS}" ]]
    then
	"${EMACSCLIENT}" -a emacs "${1}"
    else
	"${EMACSCLIENT}" -t -a emacs "${1}"
    fi
}


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
xssh() {
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
	    -X \
	    $@
    else
	"${SSH}" -X $@
    fi
}

#if [[ -d "/mnt/data3/Programs/bin" ]]; then
#    export PATH="/mnt/data3/Programs/bin:${PATH}"
#fi

## Set prompt

export EDITOR="emacsclient"
export BROWSER="firefox-nightly"

## Clear scrollback for vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
fi