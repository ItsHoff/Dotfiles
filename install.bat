mklink %UserProfile%\.vimrc %UserProfile%\Dotfiles\.vimrc
mklink %UserProfile%\.gitconfig %UserProfile%\Dotfiles\.gitconfig
del %UserProfile%\AppData\Roaming\ConEmu.xml
mklink %UserProfile%\AppData\Roaming\ConEmu.xml %UserProfile%\Dotfiles\ConEmu.xml
mklink /D %UserProfile%\.emacs.d %UserProfile%\Dotfiles\.emacs.d
git clone https://github.com/VundleVim/Vundle.vim.git %UserProfile%\.vim\bundle\Vundle.vim
