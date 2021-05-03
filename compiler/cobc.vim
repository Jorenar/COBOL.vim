" Vim compiler file
" Compiler:   GnuCOBOL
" Maintainer: Jorengarenar <dev@joren.ga>

if exists("current_compiler") | finish | endif
let current_compiler = "cobc"

let s:cpo_save = &cpo | set cpo&vim

if get(g:, "cobol_comp_mp_cobc", 0)
  CompilerSet makeprg=cobc\ -O\ -x
elseif !empty(get(g:, "cobol_comp_mp", ""))
  exec "CompilerSet makeprg=".g:cobol_comp_mp
endif

CompilerSet errorformat=%f:%\s%#%l:\ %trror:\ %m

let &cpo = s:cpo_save | unlet s:cpo_save
