" Maintainer:	Lars H. Nielsen (dengmao@gmail.com)
" Last Change:	January 22 2007
"set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "wombat_cli"
" Vim >= 7.0 specific colors
if version >= 700
hi CursorLine ctermbg=236
hi CursorColumn ctermbg=236
hi MatchParen ctermfg=7 ctermbg=none
hi Pmenu ctermfg=7 ctermbg=238
hi PmenuSel ctermfg=0 ctermbg=1
endif
" General colors
hi Cursor ctermbg=241
hi Normal ctermfg=7
hi NonText ctermfg=244 ctermbg=none
hi LineNr ctermfg=243 ctermbg=none
hi StatusLine cterm=none ctermfg=4 ctermbg=8
hi StatusLineNC cterm=none ctermfg=4 ctermbg=8
hi VertSplit ctermfg=238 ctermbg=238
hi Folded ctermbg=238 ctermfg=248
hi Title ctermfg=7
hi Visual ctermfg=7 ctermbg=238
hi SpecialKey ctermfg=244 ctermbg=236
" Syntax highlighting
hi Comment    ctermfg=246
hi Todo       ctermfg=245
hi Constant   ctermfg=173
hi String     ctermfg=113
hi Identifier ctermfg=186
hi Function   ctermfg=186
hi Type       ctermfg=186
hi Statement  ctermfg=117
hi Keyword    ctermfg=117
hi PreProc    ctermfg=173
hi Number     ctermfg=173
hi Special    ctermfg=194
hi Search     ctermbg=none ctermfg=none cterm=reverse
