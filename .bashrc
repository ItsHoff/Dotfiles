# Enable forward i-search with C-s
# https://stackoverflow.com/a/791800
[[ $- == *i* ]] && stty -ixon

# Negative globs
# https://stackoverflow.com/questions/216995/how-can-i-use-inverse-or-negative-wildcards-when-pattern-matching-in-a-unix-linu
shopt -s extglob

# Prompt
MAIN_COLOR="\[$(tput setaf 4)\]"
ACCENT_COLOR="\[$(tput setaf 9)\]"
RESET="\[$(tput sgr0)\]"

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    export PS1="${MAIN_COLOR}\u@\h: \w\n ${ACCENT_COLOR}$ ${RESET}"
else
    export PS1="${MAIN_COLOR}\w\n ${ACCENT_COLOR}$ ${RESET}"
fi
