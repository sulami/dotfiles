" Vim
set number
set cursorline
set syntax=on
set colorcolumn=80
set winheight=5
set winminheight=5
set winheight=999
set smartindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set incsearch
set ignorecase smartcase
set nobackup
set noswapfile
set encoding=utf-8
set fileformat=unix
set fileformats=unix,dos
set autoread
set foldmethod=manual
set nofoldenable
set mouse=a
set ttymouse=xterm2
colorscheme jellybeans
silent

" Tab highlights
set list
set listchars=tab:\|\ 

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" Mutt text-width
au BufRead /tmp/mutt-* set tw=72

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

