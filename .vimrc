set number
syntax on
colorscheme jellybeans
set mouse=a
set ttymouse=xterm2
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
silent
au BufRead /tmp/mutt-* set tw=72
execute pathogen#infect()
let g:airline_powerline_fonts = 0
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme = 'jellybeans'
set laststatus=2
set encoding=utf-8
set nobackup
set noswapfile
set colorcolumn=80
set fileformat=unix
set fileformats=unix,dos
set list
set listchars=tab:\|\ 
set pastetoggle=<Leader>p
imap jk <Esc>
map <Leader>t :tabnew<CR>
map <Leader>e :Explore<CR>
map <Leader>c :set tabstop=8<Return>:set noexpandtab<Return>:set shiftwidth=8<Return>
map <Leader>p :set tabstop=4<Return>:set expandtab<Return>:set shiftwidth=4<Return>
map <Leader>n :NERDTreeFind<CR>
set completeopt=longest,menuone
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' : '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

