#/bin/env sh
ln -s ~/Dotfiles/.vimrc ~/.vimrc
ln -s ~/Dotfiles/.gitconfig ~/.gitconfig
ln -s ~/Dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/Dotfiles/.emacs.d ~/
append_string="source $HOME/Dotfiles/.bash_profile"
append_file="$HOME/.bash_profile"
if ! grep -qs "$append_string" "$append_file"; then
  echo $append_string >> $append_file
fi
# git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
