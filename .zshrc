eval "$(starship init zsh)"

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
alias gb='git branch'
alias gba='git branch -a'
alias gc='git checkout '
alias df='duf'
alias docker='nerdctl'

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
# gurlo -> gurl open -> git url open
function gurlo {
    open $(gurl $1)
}

# imports
source /opt/homebrew/Cellar/git-extras/7.3.0/share/git-extras/git-extras-completion.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# kubectl aliases
alias kubectl='grc kubectl' # generic colorizer
alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'

# faster than the ketall plugin. stolen from a colleague who I won't name without checking with them first
function kga {
	  namespaced_resources() {
        \kubectl api-resources --cached --namespaced --verbs get --no-headers -o name |
        	  grep -E -v '^events(\.events\.k8s\.io)?$' | grep -E -v 'externalmetrics'
	  }
	  kubectl get $(namespaced_resources | sort | paste -sd ,) "$@" 2> >(grep -v '^Error from server (Forbidden):' >&2)
}

function wclock {

    # source https://stackoverflow.com/a/370105/1527814
    PT=`env TZ=US/Pacific date`
    CT=`env TZ=US/Central date`
    IST=`env TZ=Asia/Calcutta date`
    AT=`env TZ=Asia/Tokyo date`

    echo "Santa Clara/PT    $PT"
    echo "Central/CT        $CT"
    echo "Delhi/IST         $IST"
    echo "Tokyo/JST         $AT"

}

# prevent the vi editor problem described at https://github.com/kubernetes/website/issues/674
export KUBE_EDITOR=emacsclient

# use bat to show pretty man pages
export MANPAGER="sh -c 'col -bx | bat -l man -p'" 

eval "$(zoxide init zsh)"
