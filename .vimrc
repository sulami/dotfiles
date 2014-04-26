" Vim
set number
syntax on
set nobackup
set noswapfile
set colorcolumn=80
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set encoding=utf-8
set fileformat=unix
set fileformats=unix,dos
set mouse=a
set ttymouse=xterm2
colorscheme jellybeans
silent

" Tab highlights
set list
set listchars=tab:\|\ 

" Mutt text-width
au BufRead /tmp/mutt-* set tw=72

" Pathogen
execute pathogen#infect()

" Airline
let g:airline_powerline_fonts = 0
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme = 'jellybeans'
set laststatus=2

" SingleCompile
let g:SingleCompile_usetee = 0
let g:SingleCompile_showquickfixiferror = 1

" Hotkeys
imap jk <Esc>
set pastetoggle=<Leader>p
map <Leader>t :tabnew<CR>
map <Leader>n :NERDTreeTabsToggle<CR>
map <Leader>b :TagbarToggle<CR>
map <Leader>c :SCCompile<CR>
map <Leader>r :SCCompileRun<CR>

" Autocompletion fixes
set completeopt=longest,menuone
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' : '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

