#/bin/env sh
ln -s ~/Dotfiles/.vimrc ~/.vimrc
ln -s ~/Dotfiles/.gitconfig ~/.gitconfig
ln -s ~/Dotfiles/.tmux.conf ~/.tmux.conf
ln -s ~/Dotfiles/.emacs.d ~/
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
append_string="source ~/Dotfiles/.bash_profile"
append_file="~/.bash_profile"
if ! grep -qs "$append_string" "$append_file"; then
  echo $append_string >> $append_file
fi
