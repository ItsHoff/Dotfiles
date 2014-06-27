augroup cfiles
au!
autocmd BufRead *.c,*.cc,*.C,*.cpp,*.h set tw=0 cin noic autowrite 
autocmd BufNewFile *.c,*.cc,*.C,*.cpp,*.h set tw=0 cin noic autowrite 
autocmd BufEnter *.c,*.cc,*.C,*.cpp,*.h set tw=0 cin noic autowrite 
autocmd BufRead,BufNewFile,BufEnter *.l,*.y set tw=0 smartindent noic autowrite
augroup END

augroup f77
au!
autocmd BufRead,BufNewFile,BufEnter *.f set tw=0 ic et autowrite
augroup END

augroup f90
au!
autocmd BufRead,BufNewFile,BufEnter *.f90 set tw=0 ic autowrite 
autocmd BufRead,BufNewFile *.f90 so ~/.vim/f90compl.vim 
"autocmd BufRead,BufNewFile,BufEnter *.f90 call SaneFortran('f90') 
augroup END

augroup makable
au!
autocmd BufRead *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F4> :make<CR>
autocmd BufNewFile *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F4> :make<CR>
autocmd BufEnter *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F4> :make<CR>
autocmd BufRead *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F5> :make clean<NL>
autocmd BufNewFile *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F5> :make clean<NL>
autocmd BufEnter *.c,*.cc,*.C,*.cpp,*.h,*.l,*.y,*.f,*.f90 map <F5> :make clean<NL>
augroup END

augroup scripts
au!
autocmd BufRead,BufNewFile,BufEnter *.pl set tw=0 smartindent noic 
autocmd BufRead,BufNewFile,BufEnter *.py set tw=0 smartindent noic "ts=4 sw=4
augroup END

augroup tex
au!
autocmd BufRead,BufNewFile,BufEnter *.tex set noic autowrite
autocmd BufRead,BufNewFile   *.tex so ~/.vim/latex_compl.vim
autocmd BufRead,BufNewFile *.tex map <F4> :!latex %:r<NL>
autocmd BufRead,BufNewFile *.tex map <F5> :!xdvi %:r >/dev/null 2>&1 &<NL> 
augroup END

