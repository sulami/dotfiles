set number " line numbering
set cursorline " highlight current row
set syntax=on " highlight syntax
set colorcolumn=80 " highlight col 80
set showtabline=1 " always show tabline
set scrolloff=2 " keep cursor from upper/lower end of the buffer
set winheight=5 " temp value for winminheight
set winminheight=5 " keep buffers at least 5 rows high
set winheight=999 " maximise current buffer vertically
set splitright " open vsplits on the right side
set smartindent " indent autmatically
set tabstop=4 " 1 tab = 4 spaces
set softtabstop=4 " 1 tab = 4 spaces
set shiftwidth=4 " shift by 4 spaces
set shiftround " round indents to multiples of 4 (shiftwidth)
set expandtab " expand tabs to spaces
set incsearch " search while typing
set ignorecase smartcase " ignore case if everything is lowercase
set noswapfile " no clutter
set backup " save backups
set backupdir=/tmp " keep backups in /tmp
set encoding=utf-8 " unicode ftw
set fileformat=unix " line endings
set fileformats=unix,dos " line endings
set autoread " reread changed files automatically
set foldmethod=manual " only fold when I want to
set nofoldenable " only fold when I want to
set mouse=a " enable mouse
set ttymouse=xterm2 " enable mouse
set t_Co=256 " 256 colours
" colorscheme jellybeans " dark colors
colorscheme solarized " light colors

" Solarized
let g:solarized_termcolors=256 " force 256 colour mode

" Tab highlights
set list
set listchars=tab:\|\

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" No cursoline in Gdiff
autocmd BufRead */.git//* set nocursorline

" Mutt text-width
autocmd BufRead /tmp/mutt-* set tw=72

" Pathogen
execute pathogen#infect()

" Airline
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
" let g:airline_theme = 'jellybeans'
let g:airline_theme = 'solarized'
set laststatus=2

" Hotkeys
imap jk <Esc>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <CR> :noh<CR>
let mapleader = ','
set pastetoggle=<Leader>p
map <Leader>t :tabnew<CR>
map <Leader>o :CtrlPMixed<CR>
map <Leader>f :call RenameFile()<CR>
map <Leader>rp :!python %<CR>
map <Leader>rc :!gcc -B -pipe -m64 -ansi -fPIC -g -O3 -fno-exceptions
    \ -fstack-protector -Wl,-z,relro -Wl,-z,now -fvisibility=hidden -W -Wall
    \ -Wno-unused-parameter -Wno-unused-function -Wno-unused-label
    \ -Wpointer-arith -Wformat -Wreturn-type -Wsign-compare -Wmultichar
    \ -Wformat-nonliteral -Winit-self -Wuninitialized -Wno-deprecated
    \ -Wformat-security -Werror -o %:r % && chmod +x %:r && ./%:r<CR>

" Multi-purpose tab key, credits to GRB
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

" Rename current file
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

