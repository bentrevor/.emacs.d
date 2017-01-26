if on_linux; then
    alias ls='ls --color'
    # apt-get has an old version that doesn't use -S (smart case) by default
    alias ag='ag -S'
else
    alias ls='ls -G'
fi

alias l='ls -lhpG'
alias lsa='ls -lhpA'

alias e='emacs'
alias ec='emacsclient -c'
alias erc='emacs -e "run-erc"'
alias notes='emacs -e "just-text"'

alias grep='noglob grep --color=auto'

alias t='tree -C --dirsfirst -I "coverage|build|dist|*srv" '
alias t2='t -L 2'

# make aliases work with "sudo"
alias sudo='sudo '

# escape square brackets
alias ng='noglob '

# let tmux use 256 colors
alias tmux='tmux -2'

# -g makes it global, so it can be expanded even if it isn't the first command
alias -g be='bundle exec'
alias -g sp='spring'

alias pgstart="pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start"
alias pgstop="pg_ctl -D /usr/local/bin/postgres stop -s -m fast"

alias scm='scheme-r5rs'
alias wget_mirror='wget --mirror -p --html-extension --convert-links'
alias agis='ag --ignore-dir spec'

alias hist='vim ~/terminal_histories/current'

function caly() {
    if [[ $@ == "" ]]; then
        cal 2017
    else
        cal $1 2017
    fi
}

function calm() {
    case $1 in
        1|jan|january)        jan ;;
        2|feb|february)       feb ;;
        3|mar|march)          mar ;;
        4|apr|april)          apr ;;
        5|may)                may ;;
        6|jun|june)           jun ;;
        7|jul|july)           jul ;;
        8|aug|august)         aug ;;
        9|sep|sept|september) sep ;;
        10|oct|october)       oct ;;
        11|nov|november)      nov ;;
        12|dec|december)      dec ;;
    esac
}

function jan() { caly 1 }
function feb() { caly 2 }
function mar() { caly 3 }
function apr() { caly 4 }
function may() { caly 5 }
function jun() { caly 6 }
function jul() { caly 7 }
function aug() { caly 8 }
function sep() { caly 9 }
function oct() { caly 10 }
function nov() { caly 11 }
function dec() { caly 12 }
