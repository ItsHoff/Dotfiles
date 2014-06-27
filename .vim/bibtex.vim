" Auctex-like bindings for the bibliography entries

" source ~/.Vim/bib-syntax.vim

set notimeout
set noai

inoremap <buffer> `a <CR>@article {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>  JOURNAL = {},<CR>     YEAR = {},<CR>   VOLUME = {},<CR>   NUMBER = {},<CR>    PAGES = {},<CR>}<Esc>?@<CR>f{a
inoremap <buffer> `b <CR>@book{,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>PUBLISHER = {},<CR>  ADDRESS = {},<CR>     YEAR = {},<CR>}<Esc>?@<CR>f{a
inoremap <buffer> `i <CR>@inproceedings {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>BOOKTITLE = {},<CR>     YEAR = {},<CR>PUBLISHER = {},<CR>   SERIES = {},<CR>  ADDRESS = {},<CR>    PAGES = {},<CR>   EDITOR = {},<CR>}<Esc>?@<CR>f{a

inoremap <buffer>  <C-N> <Esc>$/$\\|{<CR>a
inoremap <buffer>  <C-P> <Esc>0?^\\|{<CR>a
noremap <buffer>  j j0f{l
noremap <buffer>  k k0f{l

" F1 removes empty fields in a citation.
map <buffer> <F1> -/^}<CR>O{},<Esc>V?@<CR>:g/{},$/d<CR>
imap <buffer> <F1> <Esc>-/^}<CR>O{},<Esc>V?@<CR>:g/{},$/d<CR>

menu 40.401 Bibtex.article\ \ \ \ \ \ \ \ `a i<CR>@article {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>  JOURNAL = {},<CR>     YEAR = {},<CR>   VOLUME = {},<CR>   NUMBER = {},<CR>    PAGES = {},<CR>}<Esc>?@<CR>f{a
imenu 40.401 Bibtex.article\ \ \ \ \ \ \ \ `a <CR>@article {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>  JOURNAL = {},<CR>     YEAR = {},<CR>   VOLUME = {},<CR>   NUMBER = {},<CR>    PAGES = {},<CR>}<Esc>?@<CR>f{a
menu 40.402 Bibtex.book\ \ \ \ \ \ \ \ \ \ \ `b i<CR>@book{,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>PUBLISHER = {},<CR>  ADDRESS = {},<CR>     YEAR = {},<CR>}<Esc>?@<CR>f{a
imenu 40.402 Bibtex.book\ \ \ \ \ \ \ \ \ \ \ `b <CR>@book{,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>PUBLISHER = {},<CR>  ADDRESS = {},<CR>     YEAR = {},<CR>}<Esc>?@<CR>f{a
menu 40.403 Bibtex.inproceedings\ \ `i i<CR>@inproceedings {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>BOOKTITLE = {},<CR>     YEAR = {},<CR>PUBLISHER = {},<CR>   SERIES = {},<CR>  ADDRESS = {},<CR>    PAGES = {},<CR>   EDITOR = {},<CR>}<Esc>?@<CR>f{a
imenu 40.403 Bibtex.inproceedings\ \ `i <CR>@inproceedings {,<CR>   AUTHOR = {},<CR>    TITLE = {},<CR>BOOKTITLE = {},<CR>     YEAR = {},<CR>PUBLISHER = {},<CR>   SERIES = {},<CR>  ADDRESS = {},<CR>    PAGES = {},<CR>   EDITOR = {},<CR>}<Esc>?@<CR>f{a
menu 40.404 Bibtex.clean\ \ \ \ \ \ \ \ \ \ F1 -/^}<CR>O{},<Esc>V?@<CR>:g/{},$/d<CR>
imenu 40.404 Bibtex.clean\ \ \ \ \ \ \ \ \ \ F1 <Esc>-/^}<CR>O{},<Esc>V?@<CR>:g/{},$/d<CR>
