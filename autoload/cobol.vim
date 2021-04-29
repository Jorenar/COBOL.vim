function! s:increase(...)
  let lnum = '.'
  let sw = shiftwidth()
  let i = a:0 ? a:1 : indent(lnum)
  if i >= 11
    return sw - (i - 11) % sw
  elseif i >= 7
    return 11-i
  elseif i == 6
    return 1
  else
    return 6-i
  endif
endfunction

function! s:decrease(...)
  let lnum = '.'
  let sw = shiftwidth()
  let i = indent(a:0 ? a:1 : lnum)
  if i >= 11 + sw
    return 1 + (i + 12) % sw
  elseif i > 11
    return i-11
  elseif i > 7
    return i-7
  elseif i == 7
    return 1
  else
    return i
  endif
endfunction

function! s:IncreaseIndent()
  let c = "\<C-T>"
  if exists("*InsertCtrlTWrapper")
    let key = InsertCtrlTWrapper()
    if key != c
      return key
    endif
  endif
  let interval = s:increase()
  let b:cobol_shiftwidth = &shiftwidth
  let &shiftwidth = 1
  let lastchar = strpart(getline('.'),col('.')-2,1)
  if lastchar == '0' || lastchar == '^'
    return "\<BS>".lastchar.c
  else
    return repeat(c,interval)
  endif
endfunction

function! s:DecreaseIndent()
  let c = "\<C-D>"
  if exists("*InsertCtrlDWrapper")
    " I hack Ctrl-D to delete when not at the end of the line.
    let key = InsertCtrlDWrapper()
    if key != c
      return key
    endif
  endif
  let interval = s:decrease()
  let b:cobol_shiftwidth = &shiftwidth
  let &shiftwidth = 1
  return repeat(c,interval)
endfunction

function! cobol#IndentBlock(shift)
  let head = strpart(getline('.'),0,7)
  let tail = strpart(getline('.'),7)
  let indent = match(tail,'[^ ]')
  let sw = shiftwidth()
  let shift = a:shift
  if shift > 0
    if indent < 4
      let tail = repeat(" ",4-indent).tail
      let shift = shift - 1
    endif
    let tail = repeat(" ",shift*sw).tail
    let shift = 0
  elseif shift < 0
    if (indent-4) > -shift * sw
      let tail = strpart(tail,-shift * sw)
    elseif (indent-4) > (-shift-1) * sw
      let tail = strpart(tail,indent - 4)
    else
      let tail = strpart(tail,indent)
    endif
  endif
  call setline('.',head.tail)
endfunction

function! cobol#IncreaseFunc(type)
  '[,']call cobol#IndentBlock(1)
endfunction

function! cobol#DecreaseFunc(type)
  '[,']call cobol#IndentBlock(-1)
endfunction

function! cobol#RestoreShiftwidth()
  if exists("b:cobol_shiftwidth")
    let &shiftwidth = b:cobol_shiftwidth
    unlet b:cobol_shiftwidth
  endif
  return ""
endfunction

function! cobol#Tab()
  if (strpart(getline('.'),0,col('.')-1) =~ '^\s*$' && &sta)
    return s:IncreaseIndent()
    " &softtabstop < 0: &softtabstop follows &shiftwidth
  elseif (&sts < 0 || &sts == shiftwidth()) && &sts != 8 && &et
    return repeat(" ",s:increase(col('.')-1))
  else
    return "\<Tab>"
  endif
endfunction
