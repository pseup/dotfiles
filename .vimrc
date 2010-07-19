" General
set nocompatible
set history=50  " keep 50 lines of command line history
set incsearch   " do incremental searching
syntax on
set hlsearch
set shell=/bin/zsh
set novb
set tf
set ic
set title
let &titleold=">"

" Display
" colorscheme wombat_cli
set ruler       " show the cursor position all the time
set showcmd     " display incomplete commands
set showmatch   " highlight matching brackets
set number      " line numbers
set scrolloff=2 " keep n lines visible above cursor if possible

" Formatting
set autoindent
set tabstop=2
set shiftwidth=2
set expandtab
set fdm=marker

" Keys
set pastetoggle=<F3>
set backspace=indent,eol,start

" Command
set wildmenu
set wildmode=list:longest,full

if has("gui_running")
    colorscheme wombat
    set guifont=Profont\ 10
    set guioptions=m
    set guicursor=a:blinkon0
    set columns=86
    set titlestring=[gvim]\ %F
elseif (&term =~ "linux")
    set t_Co=16
    set termencoding=utf-8
    set nocursorline
    colorscheme desert
else
    set t_Co=256
    colorscheme wombat_cli
    set mouse=a
    set termencoding=utf-8
    set titlestring=[vim]\ %F
    " Hightlight tabs, trailing whitespace and non breaking spaces
    set list listchars=tab:\➜\ ,trail:·,nbsp:-
endif

if has("autocmd")
  filetype plugin indent on
  augroup vimrcEx
  au!
  autocmd FileType text setlocal textwidth=78
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
  augroup END
endif " has("autocmd")

set statusline=\ %<%f\ %h%m%r%y\ [%{getcwd()}]%=%-14.(%l,%c%V%)\ %P
set laststatus=2

" C file options
au FileType c,cpp set cindent
au FileType c,cpp set formatoptions+=ro
au FileType c set omnifunc=ccomplete#Complete
au FileType cpp set omnifunc=cppcomplete#Complete

" Python file options
au FileType python set omnifunc=pythoncomplete#Complete

" Compile and run keymappings
au FileType python map <F6> :!python %<CR>
au FileType lua map <F6> :!lua %<CR>

" Highlight Redundant Whitespace
hi RedundantSpaces guibg=#303030
match RedundantSpaces /\s\+$\| \+\ze\t/

" Tabline Style
hi TabLine     ctermbg=8    ctermfg=0   cterm=NONE
hi TabLineSel  ctermbg=NONE ctermfg=4   cterm=NONE
hi TabLineFill ctermbg=8    cterm=NONE
