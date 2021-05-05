""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Adam's .vimrc
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 
" Sources: Matthias Bynens, https://github.com/mathiasbynens/dotfiles/ ;
"          Doug Black, https://dougblack.io/words/a-good-vimrc.html
" 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Make Vim more useful
set nocompatible
" Set line numbering
set number
" Enable syntax highlighting
syntax on
" Highlight current line
set cursorline
" Allow backspace in insert mode
set backspace=indent,eol,start
" Optimize for fast terminal connections
set ttyfast
" Show the current mode
set showmode
" Show the filename in the window titlebar
set title
" Enable incremental search
set incsearch
" Highlight searches
set hlsearch
" Ignore case of searches
set ignorecase
" Use UTF-8 without BOM
set encoding=utf-8 nobomb
" Set tab size to 4 spaces
set tabstop=4
" Automatic indentation
set autoindent
" Replace tabs with white spaces
set expandtab
" Show command
set showcmd
" Turn on wildmenu
set wildmenu
" Redraw only when necessary
set lazyredraw
" Show matching parenthesis-like characters
set showmatch
" Turn on Vim folding; open folds by default; maximum 10 folds deep
set foldenable
set foldlevelstart=10
set foldnestmax=10
" Turn on folding based on indentation
set foldmethod=indent
