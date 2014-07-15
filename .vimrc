set number                  " line numbering
filetype plugin on          " enable filetype detection
syntax on                   " highlight syntax
set colorcolumn=80          " highlight col 80
set cursorline              " hilight my current line
set showtabline=1           " always show tabline
set scrolloff=2             " keep cursor from upper/lower end of the buffer
set winheight=5             " temp value for winminheight
set winminheight=5          " keep buffers at least 5 rows high
set winheight=999           " maximise current buffer vertically
set splitright              " open vsplits on the right side
set showtabline=2           " always show tabline
set smartindent             " indent autmatically
set tabstop=4               " 1 tab = 4 spaces
set softtabstop=4           " 1 tab = 4 spaces
set shiftwidth=4            " shift by 4 spaces
set shiftround              " round indents to multiples of 4 (shiftwidth)
set expandtab               " expand tabs to spaces
set incsearch               " search while typing
set ignorecase smartcase    " ignore case if everything is lowercase
set noswapfile              " no clutter
set backup                  " save backups
set backupdir=/tmp          " keep backups in /tmp
set encoding=utf-8          " unicode ftw
set fileformat=unix         " line endings
set fileformats=unix,dos    " line endings
set autoread                " reread changed files automatically
set foldmethod=manual       " only fold when I want to
set nofoldenable            " only fold when I want to
set laststatus=0            " disable statusline
set t_Co=256                " 256 colours
colorscheme cybercopy       " my own colourscheme

" Tab highlights
set list
set listchars=tab:\|\ " This comment has a function...

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

" Gdiff window height fix
autocmd BufEnter */.git/index set wh=999

" Mutt text-width
autocmd BufRead /tmp/mutt-* set tw=72

" Git send-email text-width
autocmd BufRead *.gitsendemail.msg.* set tw=72

" Kernel coding style
autocmd BufEnter */linux-next/* call KernelStyle()

" Fix Markdown
autocmd BufRead *.md set ft=markdown

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
let mapleader = ','
set pastetoggle=<Leader>p
map <Leader>m <c-^>
map <Leader>t :tabnew<CR>
map <Leader>T <c-w><s-t>
map <Leader>o :CtrlPMixed<CR>
map <Leader>f :call RenameFile()<CR>
map <Leader>sk :call KernelStyle()<CR>
map <Leader>sp :call PEPStyle()<CR>
map <Leader>so i<CR>Signed-off-by: Robin Schroer <sulamiification@gmail.com>
    \<CR><Esc>
map <Leader>rl :!clear && pdflatex %<CR>
map <Leader>rm :!clear && make<CR>
map <Leader>rp :!clear && python %<CR>
map <Leader>rP :!clear && python3 %<CR>
map <Leader>rn :call RunNoseTests()<CR>
map <Leader>rN :call RunNoseTestsOnProjectRoot()<CR>
map <Leader>rd :call ProjectRootExe('!clear && python manage.py test -v 2')<CR>
map <Leader>rc :!clear && clang -Weverything -Wno-vla --std=c99 -o %:r % &&
    \ chmod +x %:r && ./%:r<CR>
map <Leader>rC :!clear && clang -g -O0 -std=c99 -Weverything -o %:r % &&
    \ chmod +x %:r && gdb -ex run ./%:r<CR>

" Predefined coding styles - Kernel
function! KernelStyle()
    exec ":set ts=8 sts=8 sw=8 noexpandtab"
endfunction

" Predefined coding styles - PEP8
function! PEPStyle()
    exec ":set ts=4 sts=4 sw=4 expandtab"
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

" Run nose on test for current file
function! RunNoseTests()
    " exec (':!clear && nosetests -v -e ' . expand('%:r') . '_test.py')
    let str = ':!cd %:h && clear && nosetests -v --exe -e %:r_test.py'
    exec str
endfunction

" Run nose on project root
function! RunNoseTestsOnProjectRoot()
    let root = ProjectRootGuess()
    exec ':!clear && find '.root.' -name "*test*.py" | xargs nosetests -v -e'
endfunction

" Project root guess-stuff, shamelessly stolen from
" github.com/vim-scripts/projectroot
if !exists('g:rootmarkers')
    let g:rootmarkers = ['.git', '.hg', '.svn', '.bzr', 'build.xml']
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

