. ~/.config/fish/aliases.fish

set -Ux EDITOR emacsclient
set -Ux fish_greeting ""
set -Ux GOPATH $HOME/go

starship init fish | source
