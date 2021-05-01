" Vim filetype plugin file
" Language:	  COBOL
" Maintainer: Jorengarenar <dev@joren.ga>
"     (formerly Tim Pope <vimNOSPAM@tpope.info>)

if exists("b:did_ftplugin") | finish | endif
let s:cpo_save = &cpo | set cpo&vim

function! s:get(var, def) abort
  return get(b:, a:var, get(g:, a:var, a:def))
endfunction

let b:cobol_autoupper    = s:get("cobol_autoupper",    0)
let b:cobol_colorcolumns = s:get("cobol_colorcolumns", 0)
let b:cobol_folding      = s:get("cobol_folding",      0)
let b:cobol_format_free  = s:get("cobol_format_free",  0)
let b:cobol_legacy_code  = s:get("cobol_legacy_code",  0)

setlocal commentstring=\ \ \ \ \ \ *%s
setlocal comments=:*:C
setlocal formatoptions+=croqlt
setlocal expandtab
setlocal iskeyword=@,48-57,-,_

let b:undo_ftplugin = "setlocal com< cms< fo< et< isk<"

if !b:cobol_format_free
  setlocal textwidth=72
  let b:undo_ftplugin .= " tw<"

  if b:cobol_colorcolumns
    setlocal colorcolumn=7,73,80
    let b:undo_ftplugin .= " cc<"
  endif
endif

if b:cobol_folding
  setlocal foldmethod=syntax
  let b:undo_ftplugin .= " fdm<"
endif

" matchit support
if !exists("b:match_words") && exists("loaded_matchit")
  let b:match_ignorecase = !b:cobol_legacy_code

  let s:ordot = '\|\ze\.\%( \@=\|$\)'
  let b:match_words=
        \ '\$IF\>:$ELSE\>:\$ENDIF\>,' .
        \ '[$-]\@<!\<if\>:\<\%(THEN\|ELSE\)\>:\<END-IF\>'.s:ordot.',' .
        \ '-\@<!\<PERFORM\s\+\%(\d\+\s\+TIMES\|UNTIL\|VARYING\|WITH\s\+TEST\)\>:\<END-PERFORM\>'.s:ordot . ',' .
        \ '-\@<!\<\%(SEARCH\|EVALUATE\)\>:\<\%(WHEN\)\>:\<END-\%(SEARCH\|EVALUATE\)\>' .s:ordot . ',' .
        \ '-\@<!\<\%(ADD\|COMPUTE\|DIVIDE\|MULTIPLY\|SUBTRACT\)\>\%(.*\(\%$\|\%(\n\%(\%(\s*\|.\{6\}\)[*/].*\n\)*\)\=\s*\%(not\s\+\)\=ON\s\+SIZE\s\+ERROR\>\)\)\@=:\%(\<NOT\s\+\)\@<!\<\%(NOT\s\+\)\=ON\s\+SIZE\s\+ERROR\>:\<END-\%(ADD\|COMPUTE\|DIVIDE\|MULTIPLY\|SUBTRACT\)\>' .s:ordot . ',' .
        \ '-\@<!\<\%(STRING\|UNSTRING\|ACCEPT\|DISPLAY\|CALL\)\>\%(.*\(\%$\|\%(\n\%(\%(\s*\|.\{6\}\)[*/].*\n\)*\)\=\s*\%(NOT\s\+\)\=ON\s\+\%(OVERFLOW\|EXCEPTION\)\>\)\)\@=:\%(\<NOT\s\+\)\@<!\<\%(NOT\s\+\)\=ON\s\+\%(OVERFLOW\|EXCEPTION\)\>:\<END-\%(STRING\|UNSTRING\|ACCEPT\|DISPLAY\|CALL\)\>' .s:ordot . ',' .
        \ '-\@<!\<\%(DELETE\|REWRITE\|START\|WRITE\|READ\)\>\%(.*\(\%$\|\%(\n\%(\%(\s*\|.\{6\}\)[*/].*\n\)*\)\=\s*\%(INVALID\s\+KEY\|AT\s\+END\|NO\s\+DATA\|AT\s\+END-OF-PAGE\)\>\)\)\@=:\%(\<NOT\s\+\)\@<!\<\%(NOT\s\+\)\=\%(INVALID\s\+KEY\|AT\s\+END\|NO\s\+DATA\|AT\s\+END-OF-PAGE\)\>:\<END-\%(DELETE\|REWRITE\|START\|WRITE\|READ\)\>' .s:ordot
endif

let b:undo_ftplugin .= " | unlet! b:match_words b:match_ignorecase b:match_skip"

" Normal mode mappings: < > << >> [[ ]] [] ][
" Visual mode mappings: < >
" Insert mode mappings: <C-T> <C-D> <Tab>
if !get(g:, "no_plugin_maps", 0) && !get(g:, "no_cobol_maps", 0)
    if version >= 700
        nnoremap <silent> <buffer> > :set opfunc=cobol#IncreaseFunc<CR>g@
        nnoremap <silent> <buffer> < :set opfunc=cobol#DecreaseFunc<CR>g@

        let b:undo_ftplugin = b:undo_ftplugin
              \ . " | sil! exe 'nunmap <buffer> >'"
              \ . " | sil! exe 'nunmap <buffer> <'"
    endif

    nnoremap <silent> <buffer> >> :call cobol#IndentBlock(1)<CR>
    nnoremap <silent> <buffer> << :call cobol#IndentBlock(-1)<CR>
    vnoremap <silent> <buffer> > :call cobol#IndentBlock(v:count1)<CR>
    vnoremap <silent> <buffer> < :call cobol#IndentBlock(-v:count1)<CR>
    inoremap <silent> <buffer> <C-T> <C-R>=cobol#IncreaseIndent()<CR><C-R>=cobol#RestoreShiftwidth()<CR>
    inoremap <silent> <buffer> <C-D> <C-R>=cobol#DecreaseIndent()<CR><C-R>=cobol#RestoreShiftwidth()<CR>

    if !maparg("<Tab>", "i")
      inoremap <silent> <buffer> <Tab> <C-R>=cobol#Tab()<CR><C-R>=cobol#RestoreShiftwidth()<CR>
      let b:undo_ftplugin .= " | sil! exe 'iunmap <buffer> <Tab>'"
    endif

    let [ s:b, s:e ] = [ '\v\c^.{6}\s+\zs', '\k+\s+%(DIVISION<Bar>SECTION)>' ]
    exec "noremap <silent> <buffer> ]] :call search('" . s:b . s:e . "','W')<CR>"
    exec "noremap <silent> <buffer> [[ :call search('" . s:b . s:e . "','bW')<CR>"
    exec "noremap <silent> <buffer> ][ :call search('" . s:b . '\k.*\n\ze\_s*' . s:e . "','W')<CR>"
    exec "noremap <silent> <buffer> [] :call search('" . s:b . '\k.*\n\ze\_s*' . s:e . "','bW')<CR>"
    unlet s:b s:e

    let b:undo_ftplugin = b:undo_ftplugin
            \ . " | sil! exe 'nunmap <buffer> >>'"
            \ . " | sil! exe 'nunmap <buffer> <<'"
            \ . " | sil! exe 'vunmap <buffer> >'"
            \ . " | sil! exe 'vunmap <buffer> <'"
            \ . " | sil! exe 'iunmap <buffer> <C-D>'"
            \ . " | sil! exe 'iunmap <buffer> <C-T>'"
            \ . " | sil! exe 'unmap  <buffer> [['"
            \ . " | sil! exe 'unmap  <buffer> ]]'"
            \ . " | sil! exe 'unmap  <buffer> []'"
            \ . " | sil! exe 'unmap  <buffer> ]['"
endif

if b:cobol_autoupper
  let g:omni_syntax_group_exclude_cobol = 'cobolBadLine,cobolComment,cobolDivision,cobolSection,cobolTodo'

  function! s:upper(k)
    if synIDattr(synID(line('.'), col('.')-1, 0), "name") !~# 'Comment\|String'
      return toupper(a:k)
    else
      return a:k " was comment or string, so don't change case
    endif
  endfunction

  function! s:init_upper() abort
    for k in syntaxcomplete#OmniSyntaxList() + [ "all", "by", "if", "to" ]
      let k = tolower(k)
      exec "iabbrev <expr> <buffer> " . k . " <SID>upper('" . k . "')"
      let b:undo_ftplugin .= " | sil! exe 'iuna <buffer> " . k ."'"
    endfor
  endfunction

  augroup COBOL_UPPER
    autocmd!
    autocmd Syntax <buffer> call <SID>init_upper()
  augroup END
endif

let b:did_ftplugin = 1
let &cpo = s:cpo_save | unlet s:cpo_save
