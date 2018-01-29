# zmodload zsh/zprof
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="afowler"
ZSH_THEME="powerlevel9k/powerlevel9k"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# plugins=(git git-prompt brew osx sublime)
plugins=(git brew osx sublime)

source $ZSH/oh-my-zsh.sh

# User configuration

# export PATH="${HOME}/bin:/usr/texbin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/Applications/Racket v6.1/bin"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# zsh-completions (brew)
fpath=(/usr/local/share/zsh-completions $fpath)

# ansible setup | installing using brew now...
# source ~/mobolt/tools/ansible/hacking/env-setup &>/dev/null

# aliases
alias mssh-add='ssh-add ~/mobolt/.mobolt-keypair/id_rsa'
alias saasdir='cd ~/mobolt/code/public/forked/fe-framework'
alias infradir='cd ~/mobolt/code/public/cloned/infra'
alias pullall='saasdir; for environ in staging production master; do git checkout $environ; git pull; done'
alias less='less -R' # colour codes are intepreted properly
alias pine='alpine'
alias pine-research='alpine -p ~sankalp/.pinerc-research'
alias perms="stat -c '%a'"
alias wget='wget -c' # resume downloads
alias vimr='vim -R'
alias 'mosh=export LC_CTYPE=en_US.UTF-8 && export LC_ALL=en_US.UTF-8 && mosh'

# indeed VPN stuff
function connectvpn(){
    ADDRESS=$1
    AUTH_GROUP=$2
    USER=$3

    sudo openconnect --reconnect-timeout 86400 \
	 --timestamp \
	 --disable-ipv6 \
	 -u "$USER" \
	 --authgroup "$AUTH_GROUP" \
	 "$ADDRESS"
}

alias vpn-disconnect='sudo pkill openconnect'
alias vpn-ops-internal='connectvpn clientvpn.indeed.com "Client VPN" sankalp'
alias vpn-ops-remote='connectvpn clientvpn.indeed.com VPN-Remote sankalp'
alias vpn-indeed-general='connectvpn corpvpn.indeed.com "VPN Client" sankalp'
alias vpn-indeed-india='connectvpn indiavpn.indeed.com "VPN Client" sankalp'
alias vpn-indeed-apac='connectvpn apacvpn.indeed.com "VPN Client" sankalp'

# OS X specific aliases
alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias ls='gls --color=auto'
alias find='gfind'
alias sed='gsed'

# # proxy settings functions
# # SRC: https://wiki.archlinux.org/index.php/proxy_settings
# function setproxy(){
#     #    echo -n "username:"
#     #    read -e username
#     #    echo -n "password:"
#     #    read -es password
#     #    export http_proxy="http://$username:$password@proxyserver:8080/"
#     echo -n "Proxy Server IP:   "
#     read PROXY_SERVER
#     echo -n "Proxy Server Port: "
#     read PROXY_PORT
#     export http_proxy="http://${PROXY_SERVER}:${PROXY_PORT}/"
#     export https_proxy=$http_proxy
#     export ftp_proxy=$http_proxy
#     export rsync_proxy=$http_proxy
#     export no_proxy="localhost,127.0.0.1"
#     #    echo -e "\nProxy environment variable set."
# }
# 
# # a version that takes 2 arguments instead of querying the user
# # useful for scripting...
# function setproxy_with_args(){
#     PROXY_SERVER=$1
#     PROXY_PORT=$2
#     export http_proxy="http://${PROXY_SERVER}:${PROXY_PORT}/"
#     export https_proxy=$http_proxy
#     export ftp_proxy=$http_proxy
#     export rsync_proxy=$http_proxy
#     export no_proxy="localhost,127.0.0.1"
# }
# 
# function unsetproxy(){
#     unset HTTP_PROXY
#     unset http_proxy
#     unset HTTPS_PROXY
#     unset https_proxy
#     unset FTP_PROXY
#     unset ftp_proxy
#     unset RSYNC_PROXY
#     unset rsync_proxy
# }
# 
# # manually set the proxy for all zsh sessions
# # setproxy_with_args 10.4.3.204 8080
# 
# # unsetting proxy for visit home...
# unsetproxy

# informative git prompt | https://github.com/olivierverdier/zsh-git-prompt
# source ~/.zsh-git-prompt/zshrc.sh
# now set the prompt
# PROMPT='[ %{$fg[magenta]%}%D %T%{$reset_color%} ] %m %{${fg_bold[blue]}%}:: %{$reset_color%}%{${fg[green]}%}%3~ $(git_super_status) %{${fg_bold[$CARETCOLOR]}%}»%{${reset_color}%}
# '

# If we're using a dumb terminal (ie. emacs), assume we don't want colour.
if [[ "$TERM" == "dumb" ]];
then
    PROMPT="%~ %# "
fi

export PATH="$PATH:$HOME/bin" # Add personal binaries to PATH

# powerlevel9k customizations
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs time)
export POWERLEVEL9K_TIME_FORMAT="%D{%H:%M:%S|%Y-%m-%d}"

# auto-complete for awscli
source /usr/local/share/zsh/site-functions/_aws

eval "$(pyenv init -)"


export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source /usr/local/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# zprof
