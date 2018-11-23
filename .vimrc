set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sleuth'
Plugin 'shougo/vimproc.vim'
Plugin 'jreybert/vimagit'
Plugin 'shougo/neocomplete.vim'
Plugin 'w0rp/ale'
Plugin 'davidhalter/jedi-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'nbouscal/vim-stylish-haskell'
call vundle#end()

filetype plugin on          " enable filetype detection
syntax on                   " highlight syntax
"set colorcolumn=80          " highlight col 80
set scrolloff=2             " keep cursor from upper/lower end of the buffer
set winheight=5             " temp value for winminheight
set winminheight=5          " keep buffers at least 5 rows high
set winheight=999           " maximise current buffer vertically
set splitbelow              " open splits below
set splitright              " open vsplits on the right side
set showtabline=1           " show the tabline when needed
set showcmd                 " show incomplete commands
set smartindent             " indent autmatically
set tabstop=2               " 1 tab = 2 spaces
set softtabstop=2           " 1 tab = 2 spaces
set shiftwidth=2            " shift by 2 spaces
set shiftround              " round indents to multiples of 4 (shiftwidth)
set expandtab               " expand tabs to spaces
set showbreak=->\           " mark wrapped lines
set nowrap                  " but don't actually wrap them
set nojoinspaces            " use a single space when joining lines
set backspace=2             " fix backspace with autoindent
set incsearch               " search while typing
set ignorecase              " ignore case
set smartcase               " if everything is lowercase
set gdefault                " add g to search/replace by default
set noswapfile              " no clutter
set backup                  " save backups
set backupdir=/tmp          " keep backups in /tmp
set backupext='.bak'        " postfix backups
set fileformat=unix         " line endings
set fileformats=unix,dos    " line endings
set autoread                " reread changed files automatically
set foldmethod=indent       " fold based on indents
set nofoldenable            " only fold when I want to
set laststatus=0            " no status line
set noshowmode              " do not show mode below statusline
set nocursorline            " no cursorline by default
set visualbell              " don't beep even if beeping is enabled
set lazyredraw              " don't draw while executing macros
set mouse=                  " mice are for plebs
set encoding=utf-8          " unicode
set t_Co=256                " all the colours
colorscheme pablo           " to then use a default scheme

" Tab highlights
set list
set listchars=tab:\>\ " This comment has a function...

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

" Highlight trailing whitespace
highlight TrailingWhitespace ctermbg=red guibg=red
match TrailingWhitespace /\s\+\%#\@<!$/

" Hotkeys
imap <c-c> <Esc>
map <Nul> <Space>
imap <Nul> <Space>
cmap <Nul> <Space>
nnoremap Q <nop>
nnoremap gh gt
nnoremap gH gT
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <Left>  :vertical resize -1<CR>
nnoremap <Right> :vertical resize +1<CR>
nnoremap <Up>    :resize +1<CR>
nnoremap <Down>  :resize -1<CR>
nnoremap <CR> :noh<CR>
let mapleader = '\'
map <Space> <Leader>
set pastetoggle=<Leader>P
nmap <Leader><Tab> <c-^>
nmap <Leader>q :q<CR>
nmap <Leader>Q :q!<CR>
nmap <Leader>m <c-^>
nmap <Leader>t :tabnew<CR>
nmap <Leader>1 1gt
nmap <Leader>2 2gt
nmap <Leader>3 3gt
nmap <Leader>4 4gt
nmap <Leader>5 5gt
nmap <Leader>6 6gt
nmap <Leader>7 7gt
nmap <Leader>8 8gt
nmap <Leader>9 9gt
nmap <Leader>0 10gt
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
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gl :Glog<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gm :Gmerge<CR>
nmap <Leader>gp :Gpush<CR>
nmap <Leader>gf :Gpull<CR>
nmap <Leader>gb :Gblame<CR>
nmap <Leader>gg :call GitGrep()<CR>
nmap <Leader>rt :call RunTestSuite()<CR>
nmap <Leader>rT :call RunTestFile()<CR>
nmap <Leader>cc :CtrlPClearAllCaches<CR>

" Writing mode
function! WritingMode()
  setl fo+=a
  setl tw=80
  setl spell
endfunction

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

" Run current test file
function! RunTestFile()
  let cur_file_type = expand('%:e')
  let test_funcs = {
    \ 'py': 'py.test %'}
  exec ':!' . expand(test_funcs[cur_file_type])
endfunction

" Run the whole project test suite
function! RunTestSuite()
  let cur_file_type = expand('%:e')
  let test_funcs = {
    \ 'py': 'py.test',
    \ 'hs': 'stack test'}
  exec ':!' . expand(test_funcs[cur_file_type])
endfunction

" Fugitive wrappers
function! GitGrep()
  let query = input('git grep: ')
  if query != ''
    exec ':Ggrep! ' . query
  endif
endfunction

" CtrlP
let g:ctrlp_custom_ignore = {
  \ 'dir' : '\v[\/](\.(git|stack-work)|target)$',
  \ 'file': '\v\.(pyc|hi|o|dyn_hi|dyn_o)$',
  \}
let g:ctrlp_prompt_mappings = {
  \ 'PrtHistory(-1)':      [],
  \ 'PrtCurEnd()':         [],
\}

" Neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1

" Jedi
let g:jedi#show_call_signatures = "2"
