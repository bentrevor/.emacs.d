
COMMAND_LOGGING=true
CUSTOM_HISTDIR=~/terminal_histories/
CUSTOM_HISTFILE=$CUSTOM_HISTDIR/current

function dont_log_that() {
    local ocl=$COMMAND_LOGGING
    export COMMAND_LOGGING=false
    echo -e '$d\n$d\nwq' | ed $CUSTOM_HISTFILE # deletes last two lines ( one is `export COMMAND_LOGGING=false`)
    export COMMAND_LOGGING=$ocl
}

touch $CUSTOM_HISTFILE
mkdir -p $CUSTOM_HISTDIR

function log_commands() {
    if [[ $(cat $CUSTOM_HISTFILE | wc -l) -gt 3000 ]]; then
        echo "logging ${CUSTOM_HISTFILE}"
        mv $CUSTOM_HISTFILE $CUSTOM_HISTDIR/$(date +%Y_%m_%d)
        touch $CUSTOM_HISTFILE
    fi
    [[ $COMMAND_LOGGING = true ]] && echo "$(date '+%Y-%m-%d\t%H:%M')\t$(pwd)\t$1" >> $CUSTOM_HISTFILE
}

function log_output() {
    if [[ "$(ps -ocommand= -p $PPID | awk '{print $1}')" != 'script' ]]; then
        timestamp=$(date +'%Y-%m-%d_%H:%M')
        logdir="$HOME/console_logs/${PWD##$HOME/}"
        logfile="$logdir/$timestamp.log"

        echo $timestamp
        echo $logdir
        echo $logfile

        mkdir -p $logdir
        script $logfile
    fi
}

# if [[ ! "$preexec_functions" == *log_output* ]]; then
#     preexec_functions+=("log_output")
# fi

if [[ ! "$preexec_functions" == *log_commands* ]]; then
    preexec_functions+=("log_commands")
fi
