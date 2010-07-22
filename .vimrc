" .vimrc
" Author Sanjoy Das <sanjoy@playingwithpointers.com>

" Debian thing.
runtime! debian.vim

" Explicitly get out of Vi compatibility
set nocompatible

" Syntax highlighting on
syntax on

" I plan to use a dark background
set background=dark

" Show command as entered in the status bar.
set showcmd

" Show matching brackets while typing.
set showmatch

" 'hello' matches both 'Hello' and 'hello' (case insensitive) while 'Hello'
" only matches 'Hello' (case sensitive).
" set ignorecase
" set smartcase
" 
" Turns out that the above options do little more than annoy me.

" Highlight as the matches are found.
set incsearch

" Smart indentation.
set autoindent
set smartindent

" Line wrapping after 80 characters.
" set textwidth=80

" 256 Colors. Yay!!
set t_Co=256

" Backup files to ~/.vim/backup
set backup
set backupdir=~/.vim/backup

" Line numbers.
set number

" Smoother page-transitions.
set scrolloff=7

" Highlight current line
set cursorline 

" So that my eyes don't burn after staring at the screen for 20 hours.
colorscheme zenburn

" Better, faster, sexier page-scrolling.
noremap <S-space> <C-b>
noremap <space> <C-f>

" Quick buffer view.
map 	<C-h> 	<Esc>:buffers<cr>
imap 	<C-h> 	:buffers<cr>

" t -> Auto-wrap lines
" c -> Auto-wrap comments
" o -> Insert current comment leader after hitting O or o
" n -> Respect numbered lists while wrapping
set formatoptions=tcon

" So that I can simply press F at a curly brace to fold the block.
map <Leader>F zfa}

" Tabs. Can't live without 'em
map <M-j> gt
map <M-k> gT

" Allow filetype specific plugins.
filetype plugin on

" I save the emails I compose at *.mail. This sets up some sane settings.
autocmd BufRead,BufNewFile   *.mail set textwidth=70
autocmd BufRead,BufNewFile   *.mail set spell

