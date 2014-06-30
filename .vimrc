set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
Plugin 'sjl/gundo.vim'
Plugin 'altercation/vim-colors-solarized'

" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'

" Git plugin not hosted on GitHub
" Plugin 'git://git.wincent.com/command-t.git'

" git repos on your local machine (i.e. when working on your own plugin)
" Plugin 'file:///home/gmarik/path/to/plugin'

" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

" Avoid a name conflict with L9
" Plugin 'user/L9', {'name': 'newL9'}

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

syntax enable
set background=dark
colorscheme solarized

set backspace=start,eol,indent
set ruler
set showcmd
set ignorecase
set vb t_vb= 
set linebreak

set listchars=tab:#^

set tabstop=8
set softtabstop=4
set shiftwidth=4
set smartindent
"To change tabs to spaces, type :%retab

nmap    <F1>    gqap
nmap    <F2>    gqqj
nmap    <F3>    kgqj
map!    <F1>    <ESC>gqapi
map!    <F2>    <ESC>gqqji
map!    <F3>    <ESC>kgqji


"map <F7> :set wm=0<nl>
"map <F8> :set wm=2<nl>
"map <F9> :set ai<nl>
"map <F10> :set noai<nl>
"map <F1> <ESC>
"map! <F1> <ESC>
"map <S-UP> <UP>
"map <S-DOWN> <DOWN>


filetype indent plugin on

source ~/.vim/autocmds.vim
source ~/.vim/vimgpg.vim
autocmd Filetype tex source ~/.vim/auctex.vim
autocmd Filetype bib source ~/.vim/bibtex.vim

function SaneFortran (foo) 
    if a:foo == 'f95' 
		let g:fortran_have_tabs=0 
		let g:fortran_more_precise=1 
		let b:fortran_free_source=1 
		let b:fortran_fixed_source=0 
		let b:fortran_dialect="f95" 
		let b:fortran_do_enddo=1 
    else 
        unlet! fortran_free_source 
    endif 
    return 0 
endfunction 

autocmd FileType cfiles set cindent
autocmd FileType fortran call SaneFortran ('f95')
autocmd FileType make set noexpandtab
autocmd FileType python set expandtab

autocmd FileType haskell set expandtab|set tabstop=8|set shiftwidth=8|set softtabstop=8


" Protect large files from sourcing and other overhead.
" Files become read only
if !exists("my_auto_commands_loaded")
        let my_auto_commands_loaded = 1
        " Large files are > 10M
        " Set options:
        "     eventignore+=FileType (no syntax highlighting etc
        "            assumes FileType always on)
        "       noswapfile (save copy of file)
        "       bufhidden=unload (save memory when other file is viewed)
        "       buftype=nowritefile (is read-only)
        "       undolevels=-1 (no undo possible)
        let g:LargeFile = 1024 * 1024 * 10
        augroup LargeFile
"                autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 | else | set eventignore-=FileType | endif
                autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload undolevels=-1 | else | set eventignore-=FileType | endif
        augroup END
endif 

