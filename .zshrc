
eval "$(starship init zsh)"

# coloured manpages
export LESS_TERMCAP_mb=$'\E[01;31m'				# begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'	# begin bold
export LESS_TERMCAP_me=$'\E[0m'						# end mode
export LESS_TERMCAP_se=$'\E[0m'						# end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'		# begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'						# end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# history-related settings for zsh
# src: https://www.soberkoder.com/better-zsh-history/
export HISTFILESIZE=1000000000
export HISTSIZE=1000000000
setopt INC_APPEND_HISTORY
export HISTTIMEFORMAT="[%F %T] "
setopt EXTENDED_HISTORY
# setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS

# normal output in ansible (not cowsay)
export ANSIBLE_NOCOWS=1

# aliases
alias ls='exa'
alias gfp='git fetch --prune'
alias df='duf'

# functions
# fzf cd
function fcd {
    cd $(fd -t d . | fzf)
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
