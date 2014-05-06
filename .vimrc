set number " line numbering
set cursorline " highlight current row
set syntax=on " highlight syntax
set colorcolumn=80 " highlight col 80
set showtabline=1 " always show tabline
set scrolloff=2 " keep cursor from upper/lower end of the buffer
set winheight=5 " temp value for winminheight
set winminheight=5 " keep buffers at least 5 rows high
set winheight=999 " maximise current buffer vertically
set smartindent " indent autmatically
set tabstop=4 " 1 tab = 4 spaces
set softtabstop=4 " 1 tab = 4 spaces
set shiftwidth=4 " shift by 4 spaces
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
colorscheme jellybeans " colorscheme

" Tab highlights
set list
set listchars=tab:\|\ 

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" Mutt text-width
autocmd BufRead /tmp/mutt-* set tw=72

" Pathogen
execute pathogen#infect()

" Airline
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_theme = 'jellybeans'
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
map <Leader>rc :!make -B %:r && ./%:r<CR>
map <Leader>rp :!python %<CR>
map <Leader>t :tabnew<CR>
map <Leader>o :CtrlPMixed<CR>
map <Leader>f :call RenameFile()<CR>

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

