filetype plugin on          " enable filetype detection
syntax on                   " highlight syntax
set number                  " line numbering
set relativenumber          " relative line numbering
set colorcolumn=80          " highlight col 80
set scrolloff=2             " keep cursor from upper/lower end of the buffer
set winheight=5             " temp value for winminheight
set winminheight=5          " keep buffers at least 5 rows high
set winheight=999           " maximise current buffer vertically
set splitbelow              " open splits below
set splitright              " open vsplits on the right side
set showtabline=2           " always show tabline
set showcmd                 " show incomplete commands
set smartindent             " indent autmatically
set tabstop=4               " 1 tab = 4 spaces
set softtabstop=4           " 1 tab = 4 spaces
set shiftwidth=4            " shift by 4 spaces
set shiftround              " round indents to multiples of 4 (shiftwidth)
set expandtab               " expand tabs to spaces
set showbreak=->\           " mark wrapped lines
set nojoinspaces            " Use a single space when joining lines
set backspace=2             " fix backspace with autoindent
set incsearch               " search while typing
set ignorecase smartcase    " ignore case if everything is lowercase
set noswapfile              " no clutter
set backup                  " save backups
set backupdir=/tmp          " keep backups in /tmp
set backupext='.bak'        " postfix backups
set encoding=utf-8          " unicode ftw
set fileformat=unix         " line endings
set fileformats=unix,dos    " line endings
set autoread                " reread changed files automatically
set foldmethod=indent       " fold based on indents
set nofoldenable            " only fold when I want to
set laststatus=2            " always show statusline (for lightline)
set noshowmode              " do not show mode below statusline
set cursorline              " cursorline by default
set visualbell              " don't beep even if beeping is enabled
set t_Co=256                " 256 colours
colorscheme su256           " colourscheme

" Gvim settings
set guioptions=agimrLt      " do not use the toolkit for the tabbar/no toolbar

" Tab highlights
set list
set listchars=tab:\>\ " This comment has a function...

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" Fix Ctrl-H in neovim
if has('nvim')
    nmap <BS> <C-w>h
endif

" Gdiff window height fix
autocmd BufEnter */.git/index set wh=999

" Mutt text-width
autocmd BufEnter /tmp/mutt-* set tw=72

" Git send-email text-width
autocmd BufEnter *.gitsendemail.msg.* set tw=72

" Kernel coding style
autocmd BufEnter *.c call KernelStyle()
autocmd BufEnter *.go call KernelStyle()

" PEP8 coding style
autocmd BufEnter *.py call PEPStyle()

" Haskell coding style
autocmd BufEnter *.hs call HaskellStyle()
autocmd BufEnter *.css call HaskellStyle()
autocmd BufEnter *.js call HaskellStyle()

" Fix Filetypes
autocmd BufEnter *.dt set ft=diet
autocmd BufEnter *.glsl set ft=c
autocmd BufEnter *.md set ft=markdown tw=79

" Cursorline always only in current window
autocmd WinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline

" Enable spellchecking for emails and (La)TeX
autocmd BufEnter /tmp/mutt-* set spell spelllang=en_us
autocmd BufEnter *.tex set spell spelllang=de_de

" Pathogen
execute pathogen#infect()

" Hotkeys
imap jk <Esc>
nnoremap gh gt
nnoremap gH gT
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <CR> :noh<CR>
map <Space> \
let mapleader = '\'
set pastetoggle=<Leader>p
map <Leader>m <c-^>
map <Leader>t :tabnew<CR>
map <Leader>T <c-w><s-t>
map <Leader>o :CtrlPMixed<CR>
map <Leader>cc :CtrlPClearCache<CR>
map <Leader>dd :call DeleteFile()<CR>
map <Leader>f :call RenameFile()<CR>
map <Leader>sk :call KernelStyle()<CR>
map <Leader>sp :call PEPStyle()<CR>
map <Leader>se :set spell spelllang=en_us<CR>
map <Leader>sd :set spell spelllang=de_de<CR>
map <Leader>so :set nospell<CR>
map <Leader>dt :diffthis<CR>
map <Leader>dp :diffput<CR>
map <Leader>tn :set number! relativenumber!<CR>
map <Leader>en :cn<CR>
map <Leader>ep :cp<CR>
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit<CR>
map <Leader>ev :tabe $MYVIMRC<CR>
map <Leader>rv :source $MYVIMRC<CR>
map <Leader>rp :call Clrun('python %')<CR>
map <Leader>rP :call ProjectRootExe('!clear && make html')<CR>
map <Leader>rh :call Clrun('ghc -O2 % && time ./%:r')<CR>
map <Leader>rH :call Clrun('ghci %')<CR>
map <Leader>rm :make<CR>
map <Leader>rM :call ProjectRootExe('!clear && make')<CR>
map <Leader>rl :call Clrun('pdflatex %')<CR>
map <Leader>ro :call Clrun('gnuplot -p %')<CR>
map <Leader>rc :call Clrun('gcc -W -Wall -O2 --std=c99 -o %:r % && time ./%:r')<CR>
map <Leader>rg :call ProjectRootExe('!clear && go build && go test -v')<CR>
map <Leader>rt :call ProjectRootExe('!clear && python setup.py test')<CR>
map <Leader>rs :call ProjectRootExe('!clear && stack build')<CR>
map <Leader>rS :call ProjectRootExe('!clear && stack test')<CR>

" Don't clear when in neovim
function! Clrun(cmd)
    if has('nvim')
        exe "!" . a:cmd
    else
        exe "!clear && " . a:cmd
    endif
endfun
command! -nargs=* -complete=command Clrun :call Clrun ('<args>')

" Dynamic Hotkeys
autocmd BufEnter *.d map <Leader>rd :call ProjectRootExe('!clear && dub')<CR>
autocmd BufEnter *.py map <Leader>rd :call ProjectRootExe('!clear && python
                          \ manage.py test -v 2')<CR>

" CtrlP
let g:ctrlp_custom_ignore = {
\   'dir' : '\v[\/]\.(git)$',
\   'file': '\v\.(pyc|hi|o)$',
\}

" UltiSnips
let g:UltiSnipsExpandTrigger="<s-tab>"

" Easymotion
nmap <Leader><Space> <Plug>(easymotion-prefix)

" Indent Guides
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 1
let g:indent_guides_guide_size = 1
let g:indent_guides_auto_colors = 0
hi IndentGuidesEven ctermfg=245 ctermbg=NONE cterm=NONE
hi IndentGuidesOdd  ctermfg=240 ctermbg=NONE cterm=NONE

" Gitgutter
hi SignColumn                   ctermbg=NONE
hi GitGutterAddDefault          ctermbg=NONE
hi GitGutterChangeDefault       ctermbg=NONE
hi GitGutterDeleteDefault       ctermbg=NONE
hi GitGutterChangeDeleteDefault ctermbg=NONE

" LightLine
let g:lightline = {
    \ 'colorscheme': 'su256',
    \ 'active': {
        \ 'left': [ [ 'mode', 'paste'],
        \           [ 'fugitive', 'readonly', 'filename', 'modified' ]]
    \ },
    \ 'component': {
        \ 'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
    \ },
    \ 'component_visible_condition': {
        \ 'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
    \ }
\ }

" Slime
let g:slime_target="tmux"

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_aggregate_errors = 1
let g:syntastic_sort_aggregated_errors = 1
let g:syntastic_haskell_checkers = ['hlint']

" Cscope
if has('cscope')
    set cscopetag cscopeverbose

    if has('quickfix')
        set cscopequickfix=s-,c-,d-,i-,t-,e-
    endif

    cnoreabbrev csa cs add
    cnoreabbrev csf cs find
    cnoreabbrev csk cs kill
    cnoreabbrev csr cs reset
    "cnoreabbrev css cs show " This is stupid...
    cnoreabbrev csh cs help

    command! -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
endif

" Predefined coding styles - Kernel
function! KernelStyle()
    exec ":set ts=8 sts=8 sw=8 noexpandtab"
endfunction

" Predefined coding styles - PEP8
function! PEPStyle()
    exec ":set ts=4 sts=4 sw=4 expandtab"
endfunction

" Predefined coding styles - Haskell
function! HaskellStyle()
    exec ":set ts=2 sts=2 sw=2 expandtab"
endfunction

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
" inoremap <s-tab> <c-n>

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

" Project root guess-stuff, shamelessly stolen from
" github.com/vim-scripts/projectroot
if !exists('g:rootmarkers')
    let g:rootmarkers = ['.git', '.hg', '.svn', 'dub.json']
endif

function! ProjectRootGuess(...)
    let fullfile = a:0 ? fnamemodify(expand(a:1), ':p') : expand('%:p')
    if exists('b:projectroot')
        if stridx(fullfile, fnamemodify(b:projectroot, ':p'))==0
            return b:projectroot
        endif
    endif
    for marker in g:rootmarkers
        let result=''
        let pivot=fullfile
        while pivot!=fnamemodify(pivot, ':h')
            let pivot=fnamemodify(pivot, ':h')
            if len(glob(pivot.'/'.marker))
                let result=pivot
            endif
        endwhile
        if len(result)
            return result
        endif
    endfor
    return filereadable(fullfile) ? fnamemodify(fullfile, ':h') : fullfile
endf

function! ProjectRootCD(...)
    let r = a:0 && len(a:1) ? ProjectRootGuess(a:1) : ProjectRootGuess()
    exe 'cd '.r
endf
command! -nargs=? -complete=file ProjectRootCD :call ProjectRootCD('<args>')

function! ProjectRootExe(cmd)
    let olddir = getcwd()
    try
        ProjectRootCD
        exe a:cmd
    finally
        exe 'cd '.olddir
    endtry
endfun
command! -nargs=* -complete=command ProjectRootExe :call
\ ProjectRootExe('<args>')

