set langmenu=en_US.UTF-8    " sets the language of the menu (gvim)
lang mes en                 " sets the language of the messages / ui (vim)
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
if has("win32") || has("win16")
    set rtp+=~/vimfiles/bundle/Vundle.vim
    let path='~/vimfiles/bundle'
else
    set rtp+=~/.vim/bundle/Vundle.vim
endif
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
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'davidhalter/jedi-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'klen/python-mode'

" Plugins to checkout
Plugin 'Raimondi/delimitMate'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
" Plugin 'Valloric/YouCompleteMe'
" Plugin 'SirVer/ultisnips'
" Plugin 'Shougo/unite.vim'

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


" MY SETTINGS

syntax enable
set background=dark
colorscheme solarized
" let g:solarized_termtrans=1
" let g:solarized_contrast="normal"
" let g:solarized_visibility="normal"
" color solarized " Load a colorscheme

" Map leader
let mapleader = "ö"

let g:jedi#auto_initialization = 1
let g:jedi#popup_select_first = 1
let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "right"
let g:jedi#show_call_signatures = 0
let g:jedi#popup_on_dot = 1

let g:pymode = 1
let g:pymode_folding = 0
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_run_bind = ''

map <C-n> :NERDTreeToggle<CR>
nnoremap <F9> :GundoToggle<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" Map esc to something else 
imap å <Esc>
vmap å <Esc>

" Make Y work like C and D
map Y y$

" Map arrow keys to swap buffers and tabs
noremap <Up> :bn<CR>
noremap <Down> :bp<CR>  
noremap <Right> gt
noremap <Left> gT

if &t_Co > 2 || has("gui_running")
    " Powerline setup
    set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9
    set laststatus=2
    set hlsearch
    set guifont=DejaVu\ Sans\ Mono\ 12,DejaVu\ LGC\ Sans\ Mono\ 12,Bitstream\ Vera\ Sans\ Mono\ 12,Nimbus\ Mono\ L\ 12
"    let g:solarized_termcolors=256
"    let g:solarized_termtrans=1
"    let g:solarized_contrast="normal"
"    let g:solarized_visibility="normal"
"    color solarized " Load a colorscheme
endif

" MY VIM SETTINGS

" Limit line-length to 80 columns by highlighting col 81 onward
if exists("+colorcolumn")
    set colorcolumn=81
endif

" Highlight current line
set cursorline
" Don’t keep results highlighted after searching...
set nohlsearch
" ...just highlight as we type
set incsearch
" Ignore case when searching...
set ignorecase
" ...except if we input a capital letter
set smartcase

" Interactions

" Start scrolling slightly before the cursor reaches an edge
set scrolloff=3
set sidescrolloff=5
" Scroll sideways a character at a time, rather than a screen at a time
set sidescroll=1
" Allow motions and back-spacing over line-endings etc
set backspace=indent,eol,start
set whichwrap=h,l,b,<,>,~,[,]

" Tabs, indentation and lines

" 4 spaces please
set expandtab
set shiftwidth=4
set tabstop=4
set softtabstop=4
" Round indent to nearest multiple of 4
set shiftround
" No line-wrapping
set nowrap

" Allow hidden buffers, don't limit to 1 file per window/split
set hidden

" " Save backups and swap files in single folder.
" set backupdir=~/.vim/backup/
" set directory=~/.vim/backup/

set history=100
set showcmd
set number
set encoding=utf-8

" DEFAULTS

"set backspace=start,eol,indent
"set ruler
"set showcmd
"set ignorecase
"set vb t_vb= 
"set linebreak

"set listchars=tab:#^

" set tabstop=8
" set softtabstop=4
" set shiftwidth=4
" set smartindent
"To change tabs to spaces, type :%retab

" filetype indent plugin on

" source ~/.vim/autocmds.vim
"source ~/.vim/vimgpg.vim
"autocmd Filetype tex source ~/.vim/auctex.vim
"autocmd Filetype bib source ~/.vim/bibtex.vim

"function SaneFortran (foo) 
"    if a:foo == 'f95' 
"		let g:fortran_have_tabs=0 
"		let g:fortran_more_precise=1 
"		let b:fortran_free_source=1 
"		let b:fortran_fixed_source=0 
"		let b:fortran_dialect="f95" 
"		let b:fortran_do_enddo=1 
"     else 
"         unlet! fortran_free_source 
"     endif 
"     return 0 
" endfunction 

" autocmd FileType cfiles set cindent
" autocmd FileType fortran call SaneFortran ('f95')
" autocmd FileType make set noexpandtab
" autocmd FileType python set expandtab

" autocmd FileType haskell set expandtab|set tabstop=8|set shiftwidth=8|set softtabstop=8


" Protect large files from sourcing and other overhead.
" Files become read only
"if !exists("my_auto_commands_loaded")
"        let my_auto_commands_loaded = 1
"        " Large files are > 10M
"        " Set options:
"        "     eventignore+=FileType (no syntax highlighting etc
"        "            assumes FileType always on)
"        "       noswapfile (save copy of file)
"        "       bufhidden=unload (save memory when other file is viewed)
"        "       buftype=nowritefile (is read-only)
"        "       undolevels=-1 (no undo possible)
"        let g:LargeFile = 1024 * 1024 * 10
"        augroup LargeFile
"                autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload buftype=nowrite undolevels=-1 | else | set eventignore-=FileType | endif
"                autocmd BufReadPre * let f=expand("<afile>") | if getfsize(f) > g:LargeFile | set eventignore+=FileType | setlocal noswapfile bufhidden=unload undolevels=-1 | else | set eventignore-=FileType | endif
"        augroup END
"endif 

