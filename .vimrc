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
set tabstop=2               " 1 tab = 4 spaces
set softtabstop=2           " 1 tab = 4 spaces
set shiftwidth=2            " shift by 4 spaces
set shiftround              " round indents to multiples of 4 (shiftwidth)
set expandtab               " expand tabs to spaces
set showbreak=->\           " mark wrapped lines
set nojoinspaces            " Use a single space when joining lines
set backspace=2             " fix backspace with autoindent
set incsearch               " search while typing
set ignorecase smartcase    " ignore case if everything is lowercase
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

if !has('nvim')
  set encoding=utf-8
endif

autocmd BufEnter * set wiw=85

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
augroup stylish-haskell
  autocmd!
  autocmd BufWritePost *.hs call s:StylishHaskell()
augroup END

" Fix Filetypes
autocmd BufEnter *.dt set ft=diet
autocmd BufEnter *.glsl set ft=c
autocmd BufEnter *.md set ft=markdown tw=79

" Cursorline always only in current window
autocmd WinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline

" Enable spellchecking for markdown, emails and (La)TeX
autocmd BufEnter *.md        set spell
autocmd BufEnter /tmp/mutt-* set spell
autocmd BufEnter *.tex       set spell

" Pathogen
execute pathogen#infect()

" Hotkeys
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
nnoremap <CR> :silent! noh \| GhcModTypeClear<CR>
map <Space> \
let mapleader = '\'
set pastetoggle=<Leader>P
nmap <Leader>y "*y
nmap <Leader>p "*p
nmap <Leader>m <c-^>
nmap <Leader>t :tabnew<CR>
nmap <Leader>T <c-w><s-t>
nmap <Leader>cc :CtrlPClearCache<CR>
nmap <Leader>dd :call DeleteFile()<CR>
nmap <Leader>f :call RenameFile()<CR>
nmap <Leader>se :set spell spelllang=en_us<CR>
nmap <Leader>sd :set spell spelllang=de_de<CR>
nmap <Leader>ss :set spell!<CR>
nmap <Leader>dt :diffthis<CR>
nmap <Leader>dp :diffput<CR>
nmap <Leader>tn :set number! relativenumber!<CR>
nmap <Leader>'  :silent! ll \| silent! cc<CR>
nmap <Leader>[  :silent! lpr \| silent! cp<CR>
nmap <Leader>]  :silent! lne \| silent! cn<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>ve :tabe $MYVIMRC<CR>
nmap <Leader>vr :source $MYVIMRC<CR>
nmap <Leader>rm :make<CR>
nmap <Leader>rM :Make<CR>
nmap <Leader>ht :GhcModType<CR>
nmap <Leader>hb :GhcModCheck<CR>

" CtrlP
let g:ctrlp_custom_ignore = {
\ 'dir' : '\v[\/]\.(git|stack-work)$',
\ 'file': '\v\.(pyc|hi|o|dyn_hi|dyn_o)$',
\}
let g:ctrlp_prompt_mappings = {
\ 'PrtSelectMove("j")':  ['<c-n>'],
\ 'PrtSelectMove("k")':  ['<c-e>'],
\ 'PrtHistory(-1)':      [],
\ 'PrtCurEnd()':         [],
\}

" Multiple Cursors
let g:multi_cursor_use_default_mapping = 0
let g:multi_cursor_start_key = '<Leader>n'
let g:multi_cursor_next_key = '<c-n>'
let g:multi_cursor_prev_key = '<c-p>'
let g:multi_cursor_skip_key = '<c-x>'
let g:multi_cursor_quit_key = '<Esc>'

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
    \           [ 'fugitive', 'readonly', 'filename', 'modified' ] ]
  \ },
  \ 'component': {
    \ 'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
  \ },
  \ 'component_visible_condition': {
    \ 'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
  \ },
  \ 'tabline': {
    \ 'left': [ [ 'tabs' ] ],
    \ 'right': [ ]
  \ },
  \ 'tab': {
    \ 'active': [ 'tabnum', 'filename', 'modified' ],
    \ 'inactive': [ 'tabnum', 'filename', 'modified' ]
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

" Supertab
let g:SuperTabDefaultCompletionType = "<c-n>"

" Deoplete
let g:deoplete#enable_at_startup = 1

" GutenTags
let g:gutentags_project_info = []
call add(g:gutentags_project_info, {'type': 'haskell', 'glob': '*.cabal'})
call add(g:gutentags_project_info, {'type': 'haskell', 'file': 'stack.yaml'})
let g:gutentags_ctags_executable_haskell = expand('~/nvim/tools/hasktags-wrapper')

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

function! MinimalMode()
  set laststatus=0
  set showtabline=0
  set nonumber
  set norelativenumber
  set ruler
  set showcmd
  set showmode
  set nocursorline
  GitGutterDisable
endfun

