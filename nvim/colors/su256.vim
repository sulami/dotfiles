" su256 vim colorscheme
" written by Robin 'sulami' Schroer
" licensed under the ISC license

set background=dark
if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

set t_Co=256
let g:colors_name = "su256"

"hi CTagsMember -- no settings --
"hi CTagsGlobalConstant -- no settings --
"hi Ignore -- no settings --
"hi CTagsImport -- no settings --
"hi CTagsGlobalVariable -- no settings --
"hi EnumerationValue -- no settings --
"hi Union -- no settings --
"hi EnumerationName -- no settings --
"hi DefinedName -- no settings --
"hi LocalVariable -- no settings --
"hi CTagsClass -- no settings --
"hi clear -- no settings --

" Base
hi Normal guifg=#d0d0d0 guibg=#000000 guisp=#000000 gui=NONE ctermfg=252 ctermbg=16 cterm=NONE
hi Cursor guifg=#192224 guibg=#F9F9F9 guisp=#F9F9F9 gui=NONE ctermfg=235 ctermbg=15 cterm=NONE
hi CursorIM guifg=#192224 guibg=#536991 guisp=#536991 gui=NONE ctermfg=235 ctermbg=15 cterm=NONE

" Various interface stuff
hi LineNr guifg=#3a3a3a guibg=NONE guisp=NONE gui=NONE ctermfg=237 ctermbg=NONE cterm=NONE
hi NonText guifg=#5E6C70 guibg=NONE guisp=NONE gui=italic ctermfg=66 ctermbg=NONE cterm=NONE
hi SignColumn guifg=#192224 guibg=#536991 guisp=#536991 gui=NONE ctermfg=235 ctermbg=NONE cterm=NONE
hi CursorLine guifg=NONE guibg=#1c1c1c guisp=#1c1c1c gui=NONE ctermfg=NONE ctermbg=233 cterm=NONE
hi CursorColumn guifg=NONE guibg=#222E30 guisp=#222E30 gui=NONE ctermfg=NONE ctermbg=233 cterm=NONE
hi ColorColumn guifg=NONE guibg=#222E30 guisp=#222E30 gui=NONE ctermfg=NONE ctermbg=233 cterm=NONE
hi ErrorMsg guifg=#ff5f5f guibg=#000000 guisp=#000000 gui=NONE ctermfg=203 ctermbg=NONE cterm=NONE
hi ModeMsg guifg=#b2b2b2 guibg=#000000 guisp=#000000 gui=bold ctermfg=249 ctermbg=NONE cterm=bold
hi MoreMsg guifg=#BD9800 guibg=NONE guisp=NONE gui=bold ctermfg=1 ctermbg=NONE cterm=bold
hi WarningMsg guifg=#A1A6A8 guibg=#912C00 guisp=#912C00 gui=NONE ctermfg=248 ctermbg=88 cterm=NONE
hi Question guifg=#b2b2b2 guibg=#000000 guisp=#000000 gui=bold ctermfg=249 ctermbg=NONE cterm=bold
hi WildMenu guifg=NONE guibg=#A1A6A8 guisp=#A1A6A8 gui=NONE ctermfg=NONE ctermbg=248 cterm=NONE
hi VertSplit guifg=#1c1c1c guibg=#000000 guisp=#000000 gui=bold ctermfg=234 ctermbg=NONE cterm=bold
hi Folded guifg=#192224 guibg=#A1A6A8 guisp=#A1A6A8 gui=italic ctermfg=237 ctermbg=NONE cterm=NONE
hi FoldColumn guifg=#192224 guibg=#A1A6A8 guisp=#A1A6A8 gui=italic ctermfg=237 ctermbg=NONE cterm=NONE
hi Visual guifg=#192224 guibg=#F9F9FF guisp=#F9F9FF gui=NONE ctermfg=NONE ctermbg=234 cterm=NONE
hi VisualNOS guifg=#192224 guibg=#F9F9FF guisp=#F9F9FF gui=underline ctermfg=235 ctermbg=189 cterm=underline
hi MatchParen guifg=#BD9800 guibg=NONE guisp=NONE gui=bold ctermfg=1 ctermbg=NONE cterm=bold

" Statusline
hi StatusLine guifg=#d0d0d0 guibg=NONE guisp=NONE gui=bold ctermfg=252 ctermbg=NONE cterm=NONE
hi StatusLineNC guifg=#3a3a3a guibg=#000000 guisp=#000000 gui=bold ctermfg=237 ctermbg=NONE cterm=NONE

" Tabline
hi TabLine guifg=#3a3a3a guibg=#000000 guisp=#000000 gui=bold ctermfg=237 ctermbg=NONE cterm=NONE
hi TabLineFill guifg=#767676 guibg=#000000 guisp=#000000 gui=bold ctermfg=237 ctermbg=NONE cterm=NONE
hi TabLineSel guifg=#767676 guibg=#000000 guisp=#000000 gui=bold ctermfg=243 ctermbg=NONE cterm=bold

" Popup menu
hi PMenu guifg=#3a3a3a guibg=#121212 guisp=#121212 gui=NONE ctermfg=239 ctermbg=234 cterm=NONE
hi PMenuSel guifg=#767676 guibg=NONE guisp=NONE gui=NONE ctermfg=244 ctermbg=234 cterm=NONE
hi PMenuSbar guifg=NONE guibg=#121212 guisp=#121212 gui=NONE ctermfg=NONE ctermbg=234 cterm=NONE
hi PMenuThumb guifg=NONE guibg=#262626 guisp=#262626 gui=NONE ctermfg=NONE ctermbg=235 cterm=NONE

" Comments
hi Comment guifg=#767676 guibg=NONE guisp=NONE gui=italic ctermfg=243 ctermbg=NONE cterm=NONE
hi Todo guifg=#eeeeee guibg=#000000 guisp=#000000 gui=bold,italic ctermfg=255 ctermbg=NONE cterm=bold
hi SpecialComment guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=250 ctermbg=NONE cterm=NONE
hi Debug guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=250 ctermbg=NONE cterm=NONE

" Search
hi IncSearch guifg=#192224 guibg=#ffffaf guisp=#ffffaf gui=underline ctermfg=235 ctermbg=229 cterm=NONE
hi Search guifg=#080808 guibg=#ffff5f guisp=#ffff5f gui=NONE ctermfg=232 ctermbg=227 cterm=NONE

" Diff
hi DiffAdd guifg=NONE guibg=#005f00 guisp=#005f00 gui=NONE ctermfg=NONE ctermbg=22 cterm=NONE
hi DiffDelete guifg=NONE guibg=#005faf guisp=#005faf gui=NONE ctermfg=NONE ctermbg=25 cterm=NONE
hi DiffChange guifg=NONE guibg=#5f0000 guisp=#5f0000 gui=NONE ctermfg=NONE ctermbg=52 cterm=NONE
hi DiffText guifg=NONE guibg=#870000 guisp=#870000 gui=NONE ctermfg=NONE ctermbg=88 cterm=NONE

" Values :: green
hi Constant guifg=#5fafaf guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi String guifg=#87d787 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Float guifg=#A1A6A8 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Number guifg=#5fafaf guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Boolean guifg=#A1A6A8 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Character guifg=#A1A6A8 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Special guifg=#afffff guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi SpecialChar guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Delimiter guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE
hi Tag guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=114 ctermbg=NONE cterm=NONE

" Preprocessor :: purple
hi PreProc guifg=#afd7ff guibg=NONE guisp=NONE gui=NONE ctermfg=99 ctermbg=NONE cterm=NONE
hi PreCondit guifg=#ffffaf guibg=NONE guisp=NONE gui=NONE ctermfg=99 ctermbg=NONE cterm=NONE
hi Define guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=99 ctermbg=NONE cterm=NONE
hi Include guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=99 ctermbg=NONE cterm=NONE
hi Macro guifg=#BD9800 guibg=NONE guisp=NONE gui=NONE ctermfg=99 ctermbg=NONE cterm=NONE

" Flowcontrol :: yellow
hi Keyword guifg=#ff5f5f guibg=NONE guisp=NONE gui=bold ctermfg=228 ctermbg=NONE cterm=NONE
hi Conditional guifg=#ffff87 guibg=NONE guisp=NONE gui=bold ctermfg=228 ctermbg=NONE cterm=NONE
hi Repeat guifg=#ffff87 guibg=NONE guisp=NONE gui=bold ctermfg=228 ctermbg=NONE cterm=NONE
hi Statement guifg=#af5f5f guibg=NONE guisp=NONE gui=bold ctermfg=228 ctermbg=NONE cterm=NONE
hi Label guifg=#BD9800 guibg=NONE guisp=NONE gui=bold ctermfg=228 ctermbg=NONE cterm=NONE

" Functions :: blue
hi Function guifg=#5f87d7 guibg=NONE guisp=NONE gui=bold ctermfg=68 ctermbg=NONE cterm=NONE

" Types :: magenta
hi Type guifg=#8787d7 guibg=NONE guisp=NONE gui=bold ctermfg=60 ctermbg=NONE cterm=NONE
hi Typedef guifg=#536991 guibg=NONE guisp=NONE gui=bold ctermfg=60 ctermbg=NONE cterm=NONE
hi StorageClass guifg=#536991 guibg=NONE guisp=NONE gui=bold ctermfg=60 ctermbg=NONE cterm=NONE
hi Structure guifg=#536991 guibg=NONE guisp=NONE gui=bold ctermfg=60 ctermbg=NONE cterm=NONE

" Operators :: red
hi Operator guifg=#8787ff guibg=NONE guisp=NONE gui=bold ctermfg=203 ctermbg=NONE cterm=NONE
hi Exception guifg=#ffffaf guibg=NONE guisp=NONE gui=bold ctermfg=203 ctermbg=NONE cterm=NONE
hi Error guifg=#A1A6A8 guibg=#912C00 guisp=#912C00 gui=NONE ctermfg=203 ctermbg=88 cterm=NONE

" Identifier :: lightblue
hi Identifier guifg=#87afd7 guibg=NONE guisp=NONE gui=NONE ctermfg=110 ctermbg=NONE cterm=NONE
hi Underlined guifg=#F9F9FF guibg=#192224 guisp=#192224 gui=underline ctermfg=110 ctermbg=NONE cterm=underline

" Spell checking
hi SpellRare guifg=#F9F9FF guibg=#192224 guisp=#192224 gui=underline ctermfg=203 ctermbg=NONE cterm=underline
hi SpellCap guifg=#F9F9FF guibg=#192224 guisp=#192224 gui=underline ctermfg=203 ctermbg=NONE cterm=underline
hi SpellLocal guifg=#F9F9FF guibg=#192224 guisp=#192224 gui=underline ctermfg=203 ctermbg=NONE cterm=underline
hi SpellBad guifg=#F9F9FF guibg=#192224 guisp=#192224 gui=underline ctermfg=203 ctermbg=NONE cterm=underline

hi Title guifg=#afd7ff guibg=NONE guisp=NONE gui=bold ctermfg=153 ctermbg=NONE cterm=bold
hi SpecialKey guifg=#5E6C70 guibg=NONE guisp=NONE gui=italic ctermfg=66 ctermbg=NONE cterm=NONE
hi Directory guifg=#536991 guibg=NONE guisp=NONE gui=bold ctermfg=60 ctermbg=NONE cterm=bold

