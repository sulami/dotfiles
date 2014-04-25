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

" Hotkeys
imap jk <Esc>
set pastetoggle=<Leader>k
map <Leader>t :tabnew<CR>
map <Leader>c :set tabstop=8<Return>:set noexpandtab<Return>:set shiftwidth=8<Return>
map <Leader>p :set tabstop=4<Return>:set expandtab<Return>:set shiftwidth=4<Return>
map <Leader>n :NERDTreeTabsToggle<CR>
map <Leader>b :TagbarToggle<CR>

" Autocompletion fixes
set completeopt=longest,menuone
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' : '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

