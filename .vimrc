" LANGUAGE SETTINGS
set langmenu=en_US.UTF-8            " sets the language of the menu (gvim)
language messages en_US.UTF-8       " sets the language of the messages / ui (vim)

" SETUP VUNDLE
set nocompatible                    " be iMproved, required
filetype off                        " required

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
" Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
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

" BASIC SETTINGS----------------------------------------------------------------

set encoding=utf-8
set autoindent
set showmode
set showcmd
set visualbell
set history=1000
set undofile
set number
set list
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set showbreak=↪
set splitbelow
set splitright
set autowrite
set autoread
set gdefault
set textwidth=0

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

" Limit line-length to 80 columns by highlighting col 81 onward
if exists("+colorcolumn")
    set colorcolumn=81
endif

" Highlight current line
set cursorline
" Keep results highlighted after searching
set hlsearch
" Highlight as we type
set incsearch
" Ignore case when searching...
set ignorecase
" ...except if we input a capital letter
set smartcase

" INTERACTIONS

" Start scrolling slightly before the cursor reaches an edge
set scrolloff=3
set sidescrolloff=5
" Scroll sideways a character at a time, rather than a screen at a time
set sidescroll=1
" Allow motions and back-spacing over line-endings etc
set backspace=indent,eol,start
set whichwrap=b,<,>,~,[,]

" TABS, INDENTATION AND LINES

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

" Backups
set backup                        " enable backups
set noswapfile
set undodir=~/.vim/tmp/undo//     " undo files
set backupdir=~/.vim/tmp/backup// " backups
set directory=~/.vim/tmp/swap//   " swap files

" Make those folders automatically if they don't already exist.
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif


" Colorscheme
syntax enable
set background=dark
colorscheme solarized

set laststatus=2
set guifont=DejaVu\ Sans\ Mono\ 12,DejaVu\ LGC\ Sans\ Mono\ 12,Bitstream\ Vera\ Sans\ Mono\ 12,Nimbus\ Mono\ L\ 12

" GUI

if has('gui_running')
    " GUI Vim

    " Remove all the UI cruft
    set go-=T
    set go-=l
    set go-=L
    set go-=r
    set go-=R

    " Use console messages instead of popups
    set go+=c
endif

" PLUGIN SETTINGS---------------------------------------------------------------

" Jedi-vim
let g:jedi#auto_initialization = 1
let g:jedi#popup_select_first = 1
let g:jedi#use_tabs_not_buffers = 1
" let g:jedi#use_splits_not_buffers = "right"
let g:jedi#show_call_signatures = 0
let g:jedi#popup_on_dot = 1

" Pymode
let g:pymode = 0
let g:pymode_folding = 0
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_run_bind = ''

" Syntastic
let g:syntastic_aggregate_errors = 1
let g:syntastic_auto_jump = 2
let g:syntastic_python_pylint_quiet_messages = {"regex": 'C0103'}
let g:syntastic_python_pylint_args = "--max-line-length=85
                                    \ --disable=C0103,R0201"

" Nerd-commenter
let g:NERDSpaceDelims = 1

" MAPPINGS----------------------------------------------------------------------

" VIM MAPPINGS

" Map leader
let mapleader = "ö"

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

" Clear highlight
noremap <leader>f :nohlsearch<CR>

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" Source selection or line
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>

" Quick editing
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Easier split commands
nnoremap <leader>so :only<CR>
nnoremap <leader>sc :close<CR>

" Easier tab commands
nnoremap <leader>tc :tabclose<CR>
nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>to :tabonly<CR>

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" MOVEMENT

" Keep search matches in the middle of the window.
nnoremap n nzzzv
nnoremap N Nzzzv

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz
nnoremap <c-o> <c-o>zz

" H and L move to the start and end of line
noremap H ^
noremap L $
vnoremap L g_

" Heresy
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" It's 2013.
noremap j gj
noremap k gk
noremap gj j
noremap gk k

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
noremap <leader>v <C-w>v
noremap <leader>h <C-w>s

" Location list movement
nnoremap <leader>ln :lnext<CR>
nnoremap <leader>lp :lprev<CR>

" PLUGIN MAPPINGS

map <C-n> :NERDTreeToggle<CR>
nnoremap <F9> :GundoToggle<CR>
nnoremap <leader>le :Errors<CR>


" AUTOCOMMANDS------------------------------------------------------------------

" Save when losing focus
au FocusLost * :silent! wall

" Resize splits when the window is resized
au VimResized * :wincmd =

" Cursorline {{{
" Only show cursorline in the current window and in normal mode.
augroup cline
    au!
    au WinLeave,InsertEnter * set nocursorline
    au WinEnter,InsertLeave * set cursorline
augroup END

" Trailing whitespace {{{
" Only shown when not in insert mode so I don't go insane.
augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:⌴
    au InsertLeave * :set listchars+=trail:⌴
augroup END

" FILETYPES

" Python
augroup ft_python
    au!
    au FileType python set foldmethod=indent
    au FileType python set foldnestmax=2
augroup END
