# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="afowler"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git svn)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:/home/sankalp/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

# If wildcard doesn't expand, pass it on to the command
setopt no_nomatch

# http://www.refining-linux.org/archives/49/ZSH-Gem-15-Shared-history/
# # Appends every command to the history file once it is executed
setopt inc_append_history
# # Reloads the history whenever you use it
setopt share_history

# smarter (sed/regex style behaviour) file renaming
autoload -U zmv

# (try to) autocorrect commands not found in $PATH
# default enabled
# setopt correct

# export SPROMPT="Correct %R to %r? (Yes, No, Abort, Edit) "
# Colour!
autoload -U colors && colors
export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r?$reset_color (Yes, No, Abort, Edit) "

# if there are commands that are getting auto-corrected but you want them not to
# add them here!
# alias foobar="nocorrect foobar"

# user@host type prompt
export PROMPT="%n @ $PROMPT"

# prompt characters for types
# http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/#repository-types
# function prompt_char {
#     git branch >/dev/null 2>/dev/null && echo '±' && return
#     hg root >/dev/null 2>/dev/null && echo '☿' && return
#     svn info >/dev/null 2>/dev/null && echo 'ƨ' && return
#     echo '○'
# }
function prompt_char {
    git branch >/dev/null 2>/dev/null && echo 'ĝ | ' && return
    hg root >/dev/null 2>/dev/null && echo 'ĥ | ' && return
    svn info >/dev/null 2>/dev/null && echo 'ŝ | ' && return
    echo ''
}

ORIG_PROMPT="$PROMPT"
# set it when zsh initializes
export PROMPT="$(prompt_char)$ORIG_PROMPT"
# chpwd is called each time the directory is changed
# use it to refresh the dirsymbol on each directory change
function chpwd() {
    export PROMPT="$(prompt_char)$ORIG_PROMPT"
}

# aliases
alias pine='alpine'
alias pine-research='alpine -p ~sankalp/.pinerc-research'
alias perms="stat -c '%a'"
alias wget='wget -c' # resume downloads
alias vimr='vim -R'
alias tconn_on='export LD_PRELOAD=$HOME/.tconn/tconn.so'
alias tconn_off='unset LD_PRELOAD'
alias rescene='mono ~/bin/srr.exe'
alias resample='mono ~/bin/srs.exe'
alias youtube-dl='mkdir -p ~/vidz-youtube; cd ~/vidz-youtube; youtube-dl'
alias utdir="cd /opt/utorrent/"
unalias gcp

# fortune! quotes OR buddhist quotes
if [[ $TERM != "dumb" ]]
then
    if (( $RANDOM % 2 ))
    then
        fortune
    else
        display-dhammapada
    fi
fi

# for emacs tramp to work properly (server-side)
if [[ $TERM == "dumb" ]]
then
    unsetopt zle && PS1='%n @ %m $ '
fi

# make gcp get the same completion behaviour as is offered to cp #awesome
compdef _cp gcp

# proxy settings functions
# SRC: https://wiki.archlinux.org/index.php/proxy_settings
function setproxy(){
#    echo -n "username:"
#    read -e username
#    echo -n "password:"
#    read -es password
#    export http_proxy="http://$username:$password@proxyserver:8080/"
    echo -n "Proxy Server IP:   "
    read PROXY_SERVER
    echo -n "Proxy Server Port: "
    read PROXY_PORT
    export http_proxy="http://${PROXY_SERVER}:${PROXY_PORT}/"
    export https_proxy=$http_proxy
    export ftp_proxy=$http_proxy
    export rsync_proxy=$http_proxy
    export no_proxy="localhost,127.0.0.1"
#    echo -e "\nProxy environment variable set."
}

# a version that takes 2 arguments instead of querying the user
# useful for scripting...
function setproxy_with_args(){
    PROXY_SERVER=$1
    PROXY_PORT=$2
    export http_proxy="http://${PROXY_SERVER}:${PROXY_PORT}/"
    export https_proxy=$http_proxy
    export ftp_proxy=$http_proxy
    export rsync_proxy=$http_proxy
    export no_proxy="localhost,127.0.0.1"
}

function unsetproxy(){
    unset HTTP_PROXY
    unset http_proxy
    unset HTTPS_PROXY
    unset https_proxy
    unset FTP_PROXY
    unset ftp_proxy
    unset RSYNC_PROXY
    unset rsync_proxy
}

# manually set the proxy for all zsh sessions
# setproxy_with_args 10.4.3.204 8080

# unsetting proxy for visit home...
unsetproxy

