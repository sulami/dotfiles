"
" Cybercopy vim colourscheme
" by sulami
" => sulami.github.io
" based on grb-light by Gary Bernhardt
"

set background=light
hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "cybercopy"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General colors
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
hi Normal           ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi NonText          ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi CurrentWindow    ctermfg=NONE        ctermbg=NONE        cterm=NONE

hi Cursor           ctermfg=black       ctermbg=yellow      cterm=NONE
hi LineNr           ctermfg=black       ctermbg=NONE        cterm=NONE

hi VertSplit        ctermbg=NONE        ctermfg=black       cterm=NONE
hi StatusLine       ctermbg=NONE        ctermfg=black       cterm=NONE
hi StatusLineNC     ctermbg=NONE        ctermfg=black       cterm=NONE

hi TabLineSel       ctermbg=NONE        ctermfg=yellow       cterm=NONE
hi TabLine          ctermbg=NONE        ctermfg=black       cterm=NONE
hi TabLineFill      ctermbg=NONE        ctermfg=NONE        cterm=NONE

hi Folded           ctermfg=yellow      ctermbg=NONE        cterm=NONE
hi Title            ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi Visual           ctermfg=NONE        ctermbg=black       cterm=NONE

hi SpecialKey       ctermfg=black       ctermbg=NONE        cterm=NONE

hi Error            ctermfg=black       ctermbg=red         cterm=NONE
hi ErrorMsg         ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi WarningMsg       ctermfg=black       ctermbg=red         cterm=NONE

hi ModeMsg          ctermfg=yellow      ctermbg=NONE        cterm=BOLD

hi CursorLine       ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi CursorLineNr     ctermfg=yellow      ctermbg=NONE        cterm=NONE
hi CursorColumn     ctermfg=NONE        ctermbg=NONE        cterm=BOLD
hi MatchParen       ctermfg=white       ctermbg=darkgray    cterm=NONE
hi Pmenu            ctermfg=NONE        ctermbg=black       cterm=NONE
hi PmenuSel         ctermfg=black       ctermbg=yellow      cterm=NONE
hi PmenuSbar        ctermfg=NONE        ctermbg=yellow      cterm=NONE
hi Search           ctermfg=black       ctermbg=yellow      cterm=NONE
hi IncSearch        ctermfg=black       ctermbg=yellow      cterm=NONE
hi ColorColumn      ctermfg=NONE        ctermbg=black       cterm=NONE

hi DiffAdd          ctermfg=black       ctermbg=green       cterm=NONE
hi DiffChange       ctermfg=black       ctermbg=blue        cterm=NONE
hi DiffText         ctermfg=black       ctermbg=cyan        cterm=NONE
hi DiffDelete       ctermfg=black       ctermbg=red         cterm=NONE

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax highlighting of actual code
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
hi Comment          ctermfg=magenta     ctermbg=NONE        cterm=NONE
hi Todo             ctermfg=red         ctermbg=NONE        cterm=BOLD
hi Constant         ctermfg=green       ctermbg=NONE        cterm=NONE

hi Statement        ctermfg=cyan        ctermbg=NONE        cterm=NONE
hi Identifier       ctermfg=blue        ctermbg=NONE        cterm=NONE
hi Function         ctermfg=yellow      ctermbg=NONE        cterm=NONE
hi Type             ctermfg=blue        ctermbg=NONE        cterm=NONE

hi Special          ctermfg=NONE        ctermbg=NONE        cterm=NONE
hi Operator         ctermfg=cyan        ctermbg=NONE        cterm=NONE

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Dependent highlighting rules. Mostly irrelevant crap from 1972.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
hi link Character       Constant
hi link Boolean         Constant
hi link Number          Constant
hi link Float           Constant
hi link String          Constant
hi link Delimiter       Constant
hi link Keyword         Statement
hi link Repeat          Statement
hi link Label           Statement
hi link Exception       Statement
hi link Conditional     Statement
hi link Define          Statement
hi link Include         Statement
hi link Macro           Statement
hi link PreCondit       Statement
hi link PreProc         Statement
hi link StorageClass    Type
hi link Structure       Type
hi link Typedef         Type
hi link Tag             Special
hi link SpecialChar     Special
hi link SpecialComment  Special
hi link Debug           Special

