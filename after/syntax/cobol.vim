" Vim filetype plugin file
" Language:	COBOL
" Author:   Jorengarenar <dev@joren.ga>

syn keyword cobolReserved contained FUNCTION

syn clear cobolComment
syn match cobolComment '\v^%(\s|\d){6}[C*/].*$'

" FOLDING {{{1

syntax region cobolIdDivFold  transparent fold
      \ start = '\v%(IDENTIFICATION DIVISION)@<=\.'
      \ end   = '\n\ze.*DIVISION'

syn clear cobolSection
syn match cobolSection "\a\k*\w\s\+SECTION"  contained contains=cobolSectionName
syntax region cobolSectionFold  transparent fold
      \ start = '\v%(SECTION)@<=\.'
      \ end   = '\n\ze.*SECTION\.'
      \ end   = '\n\ze.*DIVISION'

syn clear cobolParagraph
syn match cobolParagraph "\v%(^((\s|\d){6}.)?)@<=\k+\ze\." contained contains=cobolParagraphName
syn region cobolParagraphFold  transparent fold
      \ start = '\v%(^%(%(\s|\d){6}.)?\k+)@<=\.$'
      \ end   = '\v\n\ze^%(%(\s|\d){6}.)?\k+\.$'
      \ end   = '\n\ze.*SECTION\.'
      \ end   = '\n\ze.*DIVISION'


function! s:foo(kw, ...) abort
  exec 'syntax region cobol' . a:kw . ' transparent fold'
        \ . ' containedin=cobolLine contains=@cobolLine,cobolComment'
        \ . ' start = "\v(<' . a:kw . ')@<=\s' . get(a:, 1, '') . '"'
        \ . ' skip  = "\v^%(%(\s|\d){6}|\s*)?[*/C].*"'
        \ . ' end   = "\ze\<END-' . a:kw . '\>"'
endfunction

call s:foo('IF')
call s:foo('PERFORM', '\ze\s*UNTIL>')
call s:foo('READ')
call s:foo('EVALUATE')
