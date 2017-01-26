
if on_linux; then
    prompt_branch_color='cyan'
    prompt_path_color='black'
else
    prompt_branch_color='yellow'
    prompt_path_color='red'
fi

function inside_git_repo() { git rev-parse --git-dir > /dev/null 2>&1 }

function current_dir()    { echo "[%{$fg_bold[$prompt_path_color]%}%~%{$reset_color%}]" }
function current_branch() {
    if inside_git_repo; then
        local branch_name=$(git rev-parse --abbrev-ref HEAD)
        echo "[%{$fg_bold[$prompt_branch_color]%}$branch_name%{$reset_color%}]"
    fi
}

export PS1='$(current_branch)$(current_dir) '
