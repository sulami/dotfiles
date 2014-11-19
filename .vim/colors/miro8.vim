" miro8 colours
" Author:  jasonwryan
" URL:     http://jasonwryan.com

set background=dark
hi clear
if exists("syntax on")
    syntax reset
endif

let g:colors_name="miro8"

hi Normal          ctermfg=white	cterm=bold
hi Ignore          ctermfg=black	cterm=bold
hi Comment         ctermfg=grey
hi LineNr          ctermfg=black	cterm=bold
hi NonText         ctermfg=black	cterm=bold
hi Float           ctermfg=yellow
hi Include         ctermfg=magenta
hi Define          ctermfg=green
hi Macro           ctermfg=magenta	cterm=bold
hi PreProc         ctermfg=green	cterm=bold
hi PreCondit       ctermfg=magenta	cterm=bold
hi Directory       ctermfg=cyan
hi SpecialKey      ctermfg=yellow	cterm=bold
hi Type            ctermfg=cyan
hi String          ctermfg=green
hi Constant        ctermfg=magenta	cterm=bold
hi Special         ctermfg=green	cterm=bold
hi SpecialChar     ctermfg=red		cterm=bold
hi Number          ctermfg=yellow	cterm=bold
hi Identifier      ctermfg=magenta	cterm=bold
hi Conditional     ctermfg=cyan		cterm=bold
hi Repeat          ctermfg=red		cterm=bold
hi Statement       ctermfg=blue
hi Label           ctermfg=magenta	cterm=bold
hi Operator        ctermfg=yellow
hi Keyword         ctermfg=red		cterm=bold   
hi StorageClass    ctermfg=yellow	cterm=bold
hi Structure       ctermfg=magenta
hi Typedef         ctermfg=cyan
hi Function        ctermfg=yellow	cterm=bold
hi Exception       ctermfg=red
hi Underlined      ctermfg=blue
hi Title           ctermfg=yellow
hi Tag             ctermfg=yellow	cterm=bold
hi Delimiter       ctermfg=blue		cterm=bold 
hi SpecialComment  ctermfg=red		cterm=bold
hi Boolean         ctermfg=yellow
hi Todo            ctermfg=red		ctermbg=None	cterm=bold
hi MoreMsg         ctermfg=magenta	ctermbg=None	cterm=bold
hi ModeMsg         ctermfg=yellow	ctermbg=None	cterm=bold
hi Debug           ctermfg=red		ctermbg=None
hi MatchParen      ctermfg=black    ctermbg=white
hi ErrorMsg        ctermfg=red	    ctermbg=None
hi WildMenu        ctermfg=magenta  ctermbg=white	cterm=bold
hi Folded          cterm=reverse 	ctermfg=cyan    ctermbg=black
hi Search          ctermfg=red	    ctermbg=white	cterm=bold
hi IncSearch       ctermfg=red	    ctermbg=white
hi WarningMsg      ctermfg=red	    ctermbg=white
hi Question        ctermfg=green    ctermbg=white	cterm=bold
hi Pmenu           ctermfg=green    ctermbg=white	cterm=bold
hi PmenuSel        ctermfg=gray	    ctermbg=red 	cterm=bold
hi Visual          ctermfg=red      ctermbg=grey	cterm=bold
hi StatusLine      ctermfg=black    ctermbg=grey
hi StatusLineNC    ctermfg=white    ctermbg=black   cterm=bold
hi CursorLine      cterm=NONE       ctermbg=NONE
hi CursorLineNr    ctermfg=yellow   ctermbg=NONE    cterm=bold

" Diff lines ---
hi DiffLine        ctermbg=yellow
hi DiffText        ctermfg=white
hi DiffAdd         ctermfg=black      ctermbg=yellow
hi DiffChange      ctermfg=black
hi DiffDelete      ctermfg=black    ctermbg=blue
"test

" Specific for Vim script  --- 
hi vimCommentTitle ctermfg=green	cterm=bold
hi vimFold         ctermfg=black    ctermbg=white	cterm=bold

" Specific for help files  --- 
hi helpHyperTextJump ctermfg=yellow	cterm=bold

" JS numbers only ---
hi javaScriptNumber ctermfg=yellow	cterm=bold

" Special for HTML ---
hi htmlTag        ctermfg=cyan
hi htmlEndTag     ctermfg=cyan
hi htmlTagName    ctermfg=yellow	cterm=bold

" Specific for Perl  --- 
hi perlSharpBang  ctermfg=green		cterm=bold	term=standout
hi perlStatement  ctermfg=magenta	cterm=bold
hi perlStatementStorage ctermfg=red
hi perlVarPlain   ctermfg=yellow
hi perlVarPlain2  ctermfg=yellow	cterm=bold

" Specific for Ruby  --- 
hi rubySharpBang  ctermfg=green		cterm=bold term=standout

" Spell checking  --- 
if version >= 700
  hi clear SpellBad
  hi clear SpellCap
  hi clear SpellRare
  hi clear SpellLocal
  hi SpellBad    ctermfg=9
  hi SpellCap    ctermfg=3    cterm=underline
  hi SpellRare   ctermfg=13   cterm=underline
  hi SpellLocal  cterm=None
endif

"endif
	
" vim: foldmethod=marker foldmarker={{{,}}}:
