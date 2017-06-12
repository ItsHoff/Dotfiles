MAIN_COLOR="\[$(tput setaf 4)\]"
ACCENT_COLOR="\[$(tput setaf 9)\]"
RESET="\[$(tput sgr0)\]"

if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    export PS1="${MAIN_COLOR}\u@\h: \w\n ${ACCENT_COLOR}$ ${RESET}"
else
    export PS1="${MAIN_COLOR}\w\n ${ACCENT_COLOR}$ ${RESET}"
fi
