function show-path() { echo $PATH | tr ':' '\n' }

function rename_box() {
    OLD_BOX_NAME=$1
    PROVIDER=$2
    NEW_BOX_NAME=$3

    echo "repackaging $OLD_BOX_NAME..."
    vagrant box repackage $OLD_BOX_NAME $PROVIDER
    echo "adding $NEW_BOX_NAME..."
    vagrant box add $NEW_BOX_NAME package.box
    rm package.box
    vagrant box remove $OLD_BOX_NAME
    echo 'Success!'
}

function pryfile() {
    filename="/Users/trevorb/pryrcs/$(basename $(pwd)).pryrc"
    if [[ -e $filename ]]; then
        vim $filename
    else
        echo "no pryrc for this dir: $filename"
    fi
}

function rc() {
    local rc_file

    case $1 in
        vim)
            rc_file=~/.vimrc
            ;;
        emacs)
            rc_file=~/.emacs.d/init.el
            ;;
        zsh)
            if [[ $2 == 'functions' ]]; then
                rc_file=~/dev-env/zsh/scripts/functions.zsh
            else
                rc_file=~/.zshrc
            fi
            ;;
        *)
            echo "unsupported $1"
            ;;
    esac

    vim $rc_file
}

function sha() {
    local commit

    if [ -z $1 ]; then
        commit=HEAD
    else
        commit=$1
    fi;

    local gitsha=$(git rev-list -1 $commit)

    echo -n $gitsha | pbcopy
    echo "pbcopied $gitsha"
}

function git() {
    if [[ $@ == *"push"* ]] && [[ $@ == *"-f"* ]] && [[ $@ == *"master"* ]]; then
        echo no
    else
        /usr/bin/git $@
    fi
}

function agh() {
    # ag through terminal history
    ag $@ ~/terminal_histories
}

function fig() {
    # find . | grep
    find . -not -path "./.git/*" | grep "$@"
}

function ir() {
    # interactive rebase
    git rebase -i HEAD~$1
}

function srvt() {
    # start a server in the background, log all output to ~/logs, and tail logfile

    local logfile="$HOME/logs/$(timestamp_prefix)_$(basename $PWD)"
    echo "logging in $logfile"

    $@ > $logfile 2>&1 &
    tail -f $logfile
}

function gspec() {
    git diff master --stat --name-only --diff-filter=d | grep '_spec.rb$'

    if [[ $1 != 'files' ]]; then
        echo ''
        sleep 0.3
        spring rspec --format doc $(git diff master --stat --name-only --diff-filter=d | grep '_spec.rb$' | paste -sd ' ' -)
    fi
}

function wip() {
    if [[ $@ == "" ]]; then
        local msg=$(date +'%H:%M:%S')
    else
        local msg="$(echo $(date +'%H:%M:%S') '==>' $@)"
    fi

    git add -A
    git commit -m $msg
}

function tag() {
    if [[ $@ == "" ]]; then
        echo 'need a tag name'
    else
        local msg="$(echo $(echo $(date +'%m-%d__%H%M_') $@) | tr ' ' '_')"

        git tag $msg
    fi
}

function start() {
    local service=$1
    local logfile="$HOME/logs/$(timestamp_prefix)_$(basename $PWD)_$service"

    clear
    echo "running $service, logging in $logfile"

    case $service in
        sidekiq)
            be sidekiq -C config/sidekiq.yml > $logfile 2>&1 &
            ;;
        redis)
            redis-server /usr/local/etc/redis.conf > $logfile 2>&1 &
            ;;
        mongo)
            mongod > $logfile 2>&1 &
            ;;
        afa)
            ant start -Denv=nextcapital > $logfile 2>&1 &
            ;;
        *)
            echo "unsupported $1"
            ;;
    esac

    tail -f $logfile
}

function d() {
    pushd "$1" 2>&1 > /dev/null
}

function pd() {
    popd 2>&1 > /dev/null
}

function pryrc() {
    vim ~/pryrcs/$(basename $(pwd)).pryrc
}

function start_service() {
    local service=$1
    local logfile="$HOME/logs/$(timestamp_prefix)_$(basename $PWD)_$service"

    case $service in
         sidekiq)
            echo "running $1, logging in $logfile"
            be sidekiq -C config/sidekiq.yml > $logfile 2>&1 &
            ;;
        redis)
            echo "running $1, logging in $logfile"
            redis-server /usr/local/etc/redis.conf > $logfile 2>&1 &
            ;;
        mongo)
            echo "running $1, logging in $logfile"
            mongod > $logfile 2>&1 &
            ;;
        afa)
            echo "running $1, logging in $logfile"
            ant start -Denv=nextcapital > $logfile 2>&1 &
            ;;
        # appserver)
        #     echo 'starting all appserver services'
        #     srv sidekiq
        #     srv redis
        #     srv mongo
        #     ;;
        *)
            echo "unsupported $1"
            ;;
    esac
}

function srv() {
    # start a server in the background, log all output to ~/logs

    # local service=$1
    # mkdir -p "$HOME/logs/$service"
    # local logfile="$HOME/logs/$service/$(timestamp_prefix)_$(basename $PWD)"

    start_service mongo
    start_service sidekiq
    start_service redis

    # jobs
}

function timestamp_prefix() {
    date +'%Y-%m-%d_%H:%M'
}

function org() {
    pushd ~/org
    local filename="$HOME/org/capture/$(timestamp_prefix)_notes.org"

    emacsclient -t $filename

    if [[ -e $filename ]]; then
        local filetitle=$(head -1 $filename | sed 's/^\** //g' | sed 's/ /_/g')
        local target="$HOME/org/capture/$filetitle.org"
        mv $filename $target
        echo "created $target"
    else
        echo "file was blank"
    fi

    popd
}

function scr() {
    script $(echo "$HOME/logs/$(timestamp_prefix)_$(basename $PWD)")
}

function pss() {
    local process_names=$(echo $@ | sed 's/ /\\|/g')
    ps aux | grep -v grep | grep "$process_names"
}

#   Originally from https://github.com/mbadolato/iTerm2-Color-Schemes
function show-terminal-colors() {
    T='gYw'   # The test text

    echo -e "\n                 40m     41m     42m     43m\
     44m     45m     46m     47m";

    for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
                       '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
                       '  36m' '1;36m' '  37m' '1;37m';
    do FG=${FGs// /}
       echo -en " $FGs \033[$FG  $T  "
       for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
       do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
       done
       echo;
    done
    echo
}

function dull_red()     { echo "$(tput setaf 1)$1$(tput sgr0)" }
function dull_green()   { echo "$(tput setaf 2)$1$(tput sgr0)" }
function dull_yellow()  { echo "$(tput setaf 3)$1$(tput sgr0)" }
function dull_blue()    { echo "$(tput setaf 4)$1$(tput sgr0)" }
function dull_magenta() { echo "$(tput setaf 5)$1$(tput sgr0)" }
function dull_cyan()    { echo "$(tput setaf 6)$1$(tput sgr0)" }
function dull_white()   { echo "$(tput setaf 7)$1$(tput sgr0)" }
function dull_black()   { echo "$(tput setaf 8)$1$(tput sgr0)" }
function hot_red()      { echo "$(tput setaf 9)$1$(tput sgr0)" }
function hot_green()    { echo "$(tput setaf 10)$1$(tput sgr0)" }
function hot_yellow()   { echo "$(tput setaf 11)$1$(tput sgr0)" }
function hot_blue()     { echo "$(tput setaf 12)$1$(tput sgr0)" }
function hot_magenta()  { echo "$(tput setaf 13)$1$(tput sgr0)" }
function hot_cyan()     { echo "$(tput setaf 14)$1$(tput sgr0)" }
function hot_white()    { echo "$(tput setaf 15)$1$(tput sgr0)" }
function hot_black()    { echo "$(tput setaf 16)$1$(tput sgr0)" }

# for debugging
function show-color-functions() {
    echo "dull_red:     \t $(dull_red     'jump in the urinal')"
    echo "dull_green:   \t $(dull_green   'and stand on your head')"
    echo "dull_yellow:  \t $(dull_yellow  'im the one thats alive')"
    echo "dull_blue:    \t $(dull_blue    'youre all dead')"
    echo "dull_magenta: \t $(dull_magenta 'lean over the bowl')"
    echo "dull_cyan:    \t $(dull_cyan    'and then take a dive')"
    echo "dull_white:   \t $(dull_white   'all of you are dead')"
    echo "dull_black:   \t $(dull_black   'i am alive')\n"
    echo "hot_red:      \t $(hot_red      'jump in the urinal')"
    echo "hot_green:    \t $(hot_green    'and stand on your head')"
    echo "hot_yellow:   \t $(hot_yellow   'im the one thats alive')"
    echo "hot_blue:     \t $(hot_blue     'youre all dead')"
    echo "hot_magenta:  \t $(hot_magenta  'lean over the bowl')"
    echo "hot_cyan:     \t $(hot_cyan     'and then take a dive')"
    echo "hot_white:    \t $(hot_white    'all of you are dead')"
    echo "hot_black:    \t $(hot_black    'i am alive')\n"
}
