filetype plugin on
syntax on
set scrolloff=2
set winheight=5
set winminheight=5
set winheight=999
set splitbelow
set splitright
set showcmd
set smartindent
set tabstop=2
set softtabstop=2
set shiftwidth=2
set shiftround
set expandtab
set showbreak=->\ 
set nojoinspaces
set backspace=2
set incsearch
set ignorecase
set gdefault
set noswapfile
set backup
set backupdir=/tmp
set backupext='.bak'
set fileformat=unix
set fileformats=unix,dos
set autoread
set foldmethod=indent
set nofoldenable
set laststatus=0
set visualbell
set encoding=utf-8
set t_Co=256
colorscheme su256

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Hotkeys
imap <c-c> <Esc>
nnoremap Q <nop>
nnoremap gh gt
nnoremap gH gT
nnoremap <a-h> <c-w>h
nnoremap <a-j> <c-w>j
nnoremap <a-k> <c-w>k
nnoremap <a-l> <c-w>l
nnoremap <Left>  :vertical resize -1<CR>
nnoremap <Right> :vertical resize +1<CR>
nnoremap <Up>    :resize +1<CR>
nnoremap <Down>  :resize -1<CR>
nnoremap <CR> :noh<CR>
map <Space> \
let mapleader = '\'
set pastetoggle=<Leader>P
nmap <Leader>q :q<CR>
nmap <Leader>Q :q!<CR>
nmap <Leader>m <c-^>
nmap <Leader>t :tabnew<CR>
nmap <Leader>T <c-w><s-t>
nmap <Leader>dd :call DeleteFile()<CR>
nmap <Leader>f :call RenameFile()<CR>
nmap <Leader>se :set spell spelllang=en_us<CR>
nmap <Leader>sd :set spell spelllang=de_de<CR>
nmap <Leader>ss :set spell!<CR>
nmap <Leader>tn :set number! relativenumber!<CR>
nmap <Leader>tw :set wrap!<CR>
nmap <Leader>ve :tabe $MYVIMRC<CR>
nmap <Leader>vr :source $MYVIMRC<CR>

" Delete current file
function! DeleteFile()
  let name = expand('%')
  exec ':silent !rm ' . name
  exec ':quit!'
  redraw!
endfunction

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

