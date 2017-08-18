# Enable forward i-search with C-s
# https://stackoverflow.com/a/791800
[[ $- == *i* ]] && stty -ixon

# Negative globs
# https://stackoverflow.com/questions/216995/how-can-i-use-inverse-or-negative-wildcards-when-pattern-matching-in-a-unix-linu
shopt -s extglob
