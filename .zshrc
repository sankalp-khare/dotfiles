
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
# ref: https://zsh.sourceforge.io/Doc/Release/Options.html
export HISTFILESIZE=200000
export HISTSIZE=200000
export SAVEHIST=100000
export HISTTIMEFORMAT="[%F %T] "
setopt SHARE_HISTORY     # all sessions write to history as and when the command happens
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE # if the first char is a space don't save command to history
setopt HIST_EXPIRE_DUPS_FIRST

# normal output in ansible (not cowsay)
export ANSIBLE_NOCOWS=1

# aliases
alias ls='lsd'
alias gfp='git fetch --prune'
alias gpfp='git pull && git fetch --prune'
alias df='duf'

# functions
# indeed cd
function icd {
    # cd $(find ~/indeed -mindepth 2 -maxdepth 2 -type d -name "$1")
    cd $(fd --exact-depth 2 -t d ... ${HOME}/indeed | fzf)
}
# fzf cd
function fcd {
    cd $(fd -t d . | fzf)
}
# gurl -> git url -> print URL of file passed as argument
function gurl {
    REPO=$(git remote get-url origin | sed 's/^git@//g' | sed 's/.git$//g' | sed 's/\:/\//g')
    FILE_PATH_IN_REPO=$(git ls-files --full-name $1)
    CURRENT_BRANCH=$(git branch --show-current)
    echo "https://${REPO}/-/tree/${CURRENT_BRANCH}/${FILE_PATH_IN_REPO}"
}

# imports
source /opt/homebrew/Cellar/git-extras/7.1.0/share/git-extras/git-extras-completion.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# kubectl aliases
alias kubectl='grc kubectl' # generic colorizer
alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'

# prevent the vi editor problem described at https://github.com/kubernetes/website/issues/674
export KUBE_EDITOR=vim
