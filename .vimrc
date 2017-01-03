runtime! autoload/pathogen.vim
if exists('g:loaded_pathogen')
  execute pathogen#infect('~/.vimbundles/{}', '~/.vim/bundle/{}')
endif
runtime! ftplugin/man.vim

syntax on
filetype plugin indent on

set visualbell

set wildmenu
set wildmode=list:longest,full

set splitright
set splitbelow

set hidden

set guifont=Menlo:h15
set guioptions-=T guioptions-=e guioptions-=L guioptions-=r
set shell=bash
set modeline

augroup vimrc
  autocmd!
  autocmd GuiEnter * set columns=120 lines=70 number
augroup END

set number                        " Show line numbers.
set formatprg=par

augroup vimrc
  autocmd!
  autocmd GuiEnter * set guifont=Menlo:h15
  autocmd FileType vim setlocal foldmethod=marker
  autocmd BufNewFile,BufReadPost *.edn,*.pxi set ft=clojure
  autocmd BufNewFile,BufReadPost *.boot set ft=clojure sw=2
augroup END

colorscheme railscasts

let g:use_cursor_shapes = 1
let g:turbux_runner = 'tslime'
let g:tslime_always_current_session = 1

if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif
