## If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Number of available package updates
alias vchup='xbps-install -nuM | wc -l'
## Update packages
alias vpu='sudo xbps-install -Su'
## Install package <package name>
alias vpi='sudo xbps-install -S'
## Remove package <package name>
alias vpr='sudo xbps-remove -R'
## Search package <package name>
alias vpq='xbps-query -Rs'
## Clean obsolete packages
alias vpc='sudo xbps-remove -Oo'

## Enable color on ls and grep
alias ls='ls --color=auto'
alias grep='grep --color=auto'

## Add .local/bin to path
export PATH="${HOME}/.local/bin:${PATH}"

## Set prompt
export PS1="\u@\[\e[37m\]\h\[\e[m\]:\[\e[32m\]\w\[\e[m\]\[\e[31m\]\\$\[\e[m\] "