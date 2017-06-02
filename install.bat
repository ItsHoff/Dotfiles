mklink %UserProfile%\.vimrc %UserProfile%\Dotfiles\.vimrc
mklink %UserProfile%\.gitconfig %UserProfile%\Dotfiles\.gitconfig
mklink /D %UserProfile%\.emacs.d %UserProfile%\Dotfiles\.emacs.d
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
