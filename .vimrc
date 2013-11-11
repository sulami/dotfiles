set number
syntax on
colo pablo
set mouse=a
set ttymouse=xterm2
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
silent
au BufRead /tmp/mutt-* set tw=72
execute pathogen#infect()
let g:airline_powerline_fonts = 1
set laststatus=2
set nobackup
set noswapfile
set colorcolumn=80
set fileformat=unix
set fileformats=unix,dos
