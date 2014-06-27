filetype off
call pathogen#infect()
call pathogen#helptags()

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

if &t_Co > 2 || has("gui_running")
    " Black on Yellow
    colorscheme professional
    " zenburn is light gray on dark gray
    " colorscheme zenburn
    set hlsearch
    set guifont=DejaVu\ Sans\ Mono\ 12,DejaVu\ LGC\ Sans\ Mono\ 12,Bitstream\ Vera\ Sans\ Mono\ 12,Nimbus\ Mono\ L\ 12
endif

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

