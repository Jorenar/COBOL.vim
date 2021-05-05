" Vim syntax file
" Language:  COBOL
" Author:    Jorengarenar <dev@joren.ga>

if exists("b:current_syntax") | finish | endif
let b:current_syntax = "cobol"

" INIT {{{1

setlocal isk=@,48-57,-,_ " important for most of the keywords to work!

function! s:get(var, def) abort
  return get(b:, a:var, get(g:, a:var, a:def))
endfunction

let b:cobol_legacy_code    = s:get("cobol_legacy_code",    0)
let b:cobol_inline_comment = s:get("cobol_inline_comment", 1) * !b:cobol_legacy_code
let b:cobol_format_free    = s:get("cobol_format_free",    0) * !b:cobol_legacy_code

let g:omni_syntax_group_exclude_cobol = ''
      \ . 'cobolSeqNumArea,cobolIndicator,cobolAB,cobolProgIdArea,cobolOver80,'
      \ . 'cobolComment,cobolTodo,cobolDebugLine,cobolPreProc,'
      \ . 'cobolDivision,cobolSection,cobolParagraph,'
      \ . 'cobolSeqNum,'
      \ . 'cobolDataLvl,'
      \ . 'cobolIndComment,cobolIndDebug,cobolIndPreProc,cobolIndCont,'
      \ . 'cobolFormatFree,cobolBadFree,'
      \ . 'cobolBad'


syn case ignore

" SYNTAX {{{1
" Areas {{{2

syn match cobolSeqNumArea  '\%<7c'
syn match cobolIndicator   '\%7c'
syn match cobolAB          '\%>7c\%<73c'    contains=@cobolCode
syn match cobolProgIdArea      '\%>72c.\%<81c'
syn match cobolOver80      '\%>80c.'

syn cluster cobolAreas contains=cobolSeqNumArea,cobolIndicator,cobolAB,cobolProgIdArea

" Indicators {{{2

syn match cobolIndComment  '[*/]'  contained containedin=cobolIndicator
syn match cobolIndComment  '^[*/]' contained containedin=cobolFormatFree

syn keyword cobolIndDebug    D     contained containedin=cobolIndicator
syn match   cobolIndDebug  '^D '   contained containedin=cobolFormatFree

syn match cobolIndPreProc  '\$'    contained containedin=cobolIndicator
syn match cobolIndPreProc  '^\$'   contained containedin=cobolFormatFree

syn match cobolIndCont     '-'     contained containedin=cobolIndicator
syn match cobolIndCont     '-'     contained containedin=cobolFormatFree


syn match cobolBad     '[^ */D$-]' contained containedin=cobolIndicator

" "Indicated" {{{2

syn match cobolComment  '[*/].*$'     contained containedin=cobolIndicator
syn match cobolComment  '^\s*[*/].*$' contained containedin=cobolFormatFree

if b:cobol_inline_comment
  syn match cobolInlineComment '\*>.*'
  syn cluster cobolCode add=cobolInlineComment
endif

syn match cobolPreProc '\$.*$' contained containedin=cobolIndicator
syn match cobolPreProc '^\s*\$.*$'   contained containedin=cobolFormatFree

syn match cobolDebugLine  '\v(%7cD\s*)@<=.*$' contains=ALLBUT,cobolBad
syn match cobolDebugLine  '^\s*D .*$'  contains=ALL contained containedin=cobolFormatFree

" DIVISION, SECTION, PRAGRAPH {{{2

syn match   cobolDivision       contained  '\a\k*\w\s\+DIVISION'  contains=cobolDivisionName
syn keyword cobolDivisionName   contained  IDENTIFICATION ENVIRONMENT DATA PROCEDURE

syn match   cobolSection        contained '\a\k*\w\s\+SECTION\ze\.'  contains=cobolSectionName
syn keyword cobolSectionName    contained  CONFIGURATION INPUT-OUTPUT FILE WORKING-STORAGE LOCAL-STORAGE LINKAGE

syn match   cobolParagraph      contained '\v%(^((\s|\d){6}.)?)@<=\w\k*\ze\.'  contains=cobolParaNameIdDiv,cobolParaNameSec
syn keyword cobolParaNameIdDiv  contained  PROGRAM-ID AUTHOR INSTALLATION SOURCE-COMPUTER OBJECT-COMPUTER DATE-WRITTEN DATE-COMPILED SECURITY
syn keyword cobolParaNameSec    contained  SPECIAL-NAMES FILE-CONTROL I-O-CONTROL

syn cluster cobolCode add=cobolDivision,cobolSection,cobolParagraph

" Strings, numbers {{{2

syn match cobolString  /"[^"]*\("\|$\)/
syn match cobolString  /'[^']*\('\|$\)/
syn cluster cobolCode add=cobolString

syn match   cobolNumber "\v<-?\d*\.?\d+>"
syn cluster cobolCode  add=cobolNumber

" Data items {{{2

syn match cobolDataLvl '\%8c\s*\d\?\d\s'
syn match cobolDataLvl '\v^\s*\d?\d\s' contained containedin=cobolFormatFree

syn match cobolPic '\v%(PIC\s*)@<=[9AXVSPZ]+(\(\d+\))?'
syn cluster cobolCode add=cobolPic

" Reserved keywords {{{2

" Taken from: https://www.ibm.com/docs/en/iis/8.5?topic=words-cobol-reserved
"    TODO: segregate into appropriate groups

syn keyword cobolReserved  contained  ACCEPT ACCESS ADDRESS ADVANCING AFTER ALL ALPHABET ALPHABETIC
syn keyword cobolReserved  contained  ALPHABETIC-LOWER ALPHABETIC-UPPER ALPHANUMERIC ALPHANUMERIC-EDITED ALSO ALTER
syn keyword cobolReserved  contained  ALTERNATE ANY APPLY ARE AREA AREAS ASCENDING ASSIGN AT BASIS BEFORE
syn keyword cobolReserved  contained  BEGINNING BINARY BLANK BLOCK BOTTOM CANCEL CBL CD CF CH CHARACTER CHARACTERS
syn keyword cobolReserved  contained  CLASS CLASS-ID CLOCK-UNITS CLOSE COBOL CODE CODE-SET COLLATING COLUMN COM-REG
syn keyword cobolReserved  contained  COMMA COMMON COMMUNICATION
syn keyword cobolReserved  contained  COMP COMP-1 COMP-2 COMP-3 COMP-4 COMP-5
syn keyword cobolReserved  contained  COMPUTATIONAL COMPUTATIONAL-1 COMPUTATIONAL-2 COMPUTATIONAL-3 COMPUTATIONAL-4 COMPUTATIONAL-5
syn keyword cobolReserved  contained  CONTENT CONTINUE CONTROL CONTROLS
syn keyword cobolReserved  contained  CONVERTING CORR CORRESPONDING COUNT CURRENCY
syn keyword cobolReserved  contained  DAY DAY-OF-WEEK DBCS DE DEBUG-CONTENTS DEBUG-ITEM DEBUG-LINE
syn keyword cobolReserved  contained  DEBUG-NAME DEBUG-SUB-1 DEBUG-SUB-2 DEBUG-SUB-3 DEBUGGING DECIMAL-POINT
syn keyword cobolReserved  contained  DECLARATIVES DELETE DELIMITED DELIMITER DEPENDING DESCENDING DESTINATION DETAIL
syn keyword cobolReserved  contained  DISPLAY DISPLAY-1 DIVISION DOWN DUPLICATES DYNAMIC EGCS EGI EJECT ELSE
syn keyword cobolReserved  contained  EMI ENABLE END END-ADD END-CALL END-COMPUTE END-DELETE END-DIVIDE
syn keyword cobolReserved  contained  END-MULTIPLY END-OF-PAGE END-PERFORM END-READ END-RECEIVE
syn keyword cobolReserved  contained  END-RETURN END-REWRITE END-SEARCH END-START END-STRING END-SUBTRACT
syn keyword cobolReserved  contained  END-UNSTRING END-WRITE ENDING ENTER ENTRY EOP EQUAL ERROR ESI
syn keyword cobolReserved  contained  EVERY EXCEPTION EXIT EXTEND EXTERNAL FD FILLER
syn keyword cobolReserved  contained  FINAL FIRST FOOTING FOR FROM GENERATE GIVING GLOBAL GOBACK GREATER
syn keyword cobolReserved  contained  GROUP HEADING I-O ID
syn keyword cobolReserved  contained  INDEX INDEXED INDICATE INHERITS INITIAL INITIALIZE INITIATE INPUT
syn keyword cobolReserved  contained  INSERT INSPECT INTO INVALID IS JUST JUSTIFIED KANJI KEY
syn keyword cobolReserved  contained  LABEL LAST LEADING LEFT LENGTH LESS LIMIT LIMITS LINAGE LINAGE-COUNTER LINE
syn keyword cobolReserved  contained  LINE-COUNTER LINES LOCK MEMORY MERGE
syn keyword cobolReserved  contained  MESSAGE METACLASS METHOD METHOD-ID MODE MODULES MORE-LABELS MOVE MULTIPLE
syn keyword cobolReserved  contained  NATIVE NATIVE_BINARY NEGATIVE NEXT NO NUMBER NUMERIC
syn keyword cobolReserved  contained  NUMERIC-EDITED OBJECT OCCURS OF OFF OMITTED ON OPEN OPTIONAL
syn keyword cobolReserved  contained  ORDER ORGANIZATION OTHER OUTPUT OVERFLOW OVERRIDE PACKED-DECIMAL PADDING PAGE
syn keyword cobolReserved  contained  PAGE-COUNTER PASSWORD PERFORM PF PH PIC PICTURE PLUS POINTER POSITION POSITIVE
syn keyword cobolReserved  contained  PRINTING PROCEDURE-POINTER PROCEDURES PROCEED PROCESSING PROGRAM
syn keyword cobolReserved  contained  PURGE QUEUE QUOTE QUOTES RANDOM RD READ READY RECEIVE RECORD RECORDING RECORDS
syn keyword cobolReserved  contained  RECURSIVE REDEFINES REEL REFERENCE REFERENCES RELATIVE RELEASE RELOAD
syn keyword cobolReserved  contained  REMAINDER REMOVAL RENAMES REPLACE REPORT REPORTING REPORTS REPOSITORY
syn keyword cobolReserved  contained  RERUN RESERVE RESET RETURN RETURN-CODE RETURNING REVERSED REWIND REWRITE RF RH
syn keyword cobolReserved  contained  RIGHT ROUNDED RUN SAME SD SEARCH SECTION SEGMENT SEGMENT-LIMIT SELECT
syn keyword cobolReserved  contained  SELF SEND SENTENCE SEPARATE SEQUENCE SEQUENTIAL SERVICE SET SHIFT-IN SHIFT-OUT
syn keyword cobolReserved  contained  SIGN SIZE SKIP1 SKIP2 SKIP3 SORT SORT-CONTROL SORT-CORE-SIZE SORT-FILE-SIZE
syn keyword cobolReserved  contained  SORT-MERGE SORT-MESSAGE SORT-MODE-SIZE SORT-RETURN SOURCE
syn keyword cobolReserved  contained  STANDARD STANDARD-1 STANDARD-2 START STATUS STOP STRING
syn keyword cobolReserved  contained  SUB-QUEUE-1 SUB-QUEUE-2 SUB-QUEUE-3 SUM SUPER SUPPRESS SYMBOLIC SYNC
syn keyword cobolReserved  contained  SYNCHRONIZED TABLE TALLY TALLYING TAPE TERMINAL TERMINATE TEST TEXT THAN THEN
syn keyword cobolReserved  contained  THROUGH TIME TITLE TOP TRACE TRAILING TYPE UNIT UNSTRING
syn keyword cobolReserved  contained  UP UPON USAGE USE USING VALUE VALUES WHEN WHEN-COMPILED WITH
syn keyword cobolReserved  contained  WORDS WRITE WRITE-ONLY

syn match   cobolReserved contained  "\<CONTAINS\>" " Impossible to set as keyword in Vim
syn match   cobolReserved contained  '\<TO\>'       " `TO` is used in many constructs (`GO TO`, `SET x TO`, ...)
syn match   cobolReserved contained  '\<GO\>'       " `GO` also used in `GO TO`

syn match   cobolReserved contained  "\<BY\>\s\+\(==\)\@!"
syn match   cobolReserved contained  "\<REPLACING\>\s\+\(==\)\@!"

syn keyword cobolArithmetic   contained  COMPUTE ADD SUBTRACT MULTIPLY DIVIDE
syn keyword cobolBoolean      contained  TRUE FALSE
syn keyword cobolBoolOperator contained  NOT AND OR
syn keyword cobolConditional  contained  IF END-IF EVALUATE END-EVALUATE
syn keyword cobolConstant     contained  SPACE SPACES NULL NULLS ZERO ZEROS ZEROES LOW-VALUE LOW-VALUES HIGH-VALUE HIGH-VALUES
syn keyword cobolExec         contained  EXEC END-EXEC
syn keyword cobolCalls        contained  FUNCTION CALL END-CALL CANCEL GOBACK PERFORM END-PERFORM INVOKE END-INVOKE THRU
syn keyword cobolRepeat       contained  UNTIL VARYING TIMES
syn keyword cobolGoTo         contained  GOTO
syn keyword cobolExtra        contained  DISABLE


syn cluster cobolCode add=cobolReserved
      \ add=cobolArithmetic
      \ add=cobolBoolean,cobolBoolOperator
      \ add=cobolConditional,cobolExec
      \ add=cobolConstant,cobolExec
      \ add=cobolCalls,cobolGoTo,cobolRepeat
      \ add=cobolExtra

let b:cobol_syntax_missing_keywords = [ "BY", "CONTAINS", "COPY", "GO", "IN", "TO" ]

" ~ {{{2

syn match cobolBadFree '\v^\s*\d{6}(\w\k*\.)@!' contained containedin=cobolFormatFree

if b:cobol_legacy_code
  syn match cobolBad '^\s\{,6}'
endif

syn match cobolTodo		'TODO' contained containedin=.*Comment

syn match cobolSeqNum '.'    contained containedin=cobolSeqNumArea

syn match cobolGoTo   '\v<GO>\s*<TO>' contained

syn match cobolCALLProg  /\v%(<CALL\s+)@<=(["']?)\k+\1/
syn match cobolFunction  /\v%(<FUNCTION\s+)@<=\k+\ze\(.*\)/

"  Highlight copybook name and location in using different group
syn match cobolCopy      "\<COPY\>\|\<IN\>"
syn match cobolCopy      "\<REPLACING\>\s\+\(==\)\@="
syn match cobolCopy      "\<BY\>\s\+\(==\)\@="
syn match cobolCopyName  "\v<COPY>\s+\w+(\s+<IN>\s+\w+)?" contains=cobolCopy

" FOLDING {{{1

syn region cobolIdDivFold      transparent fold
      \ start = '\v%(IDENTIFICATION DIVISION)@<=\.'
      \ end   = '\ze.*DIVISION'

syn region cobolSectionFold    transparent fold
      \ start = '\v%(SECTION)@<=\.'
      \ end   = '\ze.*SECTION\.'
      \ end   = '\ze.*DIVISION'

syn region cobolParagraphFold  transparent fold
      \ start = '\v%(^%(%(\s|\d){6}.)?\k+)@<=\.$'
      \ end   = '\ze\v^%(%(\s|\d){6}.)?\k+\.$'
      \ end   = '\ze.*SECTION\.'
      \ end   = '\ze.*DIVISION'


function! s:foldingTerminated(kw, ...) abort
  exec 'syntax region cobol_' . a:kw . '_Fold transparent fold'
        \ . ' start = "\v(<' . a:kw . ')@<=\s' . get(a:, 1, '') . '"'
        \ . ' skip  = "\v^%(%(\s|\d){6}|\s*)?[*/C].*"'
        \ . ' end   = "\ze\<END-' . a:kw . '\>"'
endfunction

call s:foldingTerminated('DELETE')
call s:foldingTerminated('EVALUATE')
call s:foldingTerminated('IF')
call s:foldingTerminated('PERFORM', '\ze\s*UNTIL>')
call s:foldingTerminated('READ')
call s:foldingTerminated('REWRITE')
call s:foldingTerminated('SEARCH', '\ze\s*ALL>')
call s:foldingTerminated('STRING')
call s:foldingTerminated('UNSTRING')
call s:foldingTerminated('WRITE')

syn cluster cobolFolds contains=cobol.*Fold

" FORMAT FREE {{{1

if b:cobol_format_free
  syn region cobolFormatFree keepend contains=@cobolCode,@cobolFolds
        \ start = '\%1l'
        \ start = '>>\s*SOURCE\s\?FORMAT FREE'
        \ skip  = '^[*/].*$\|\*>.*'
        \ end   = '>>\s*SOURCE\s\?FORMAT FIXED'
else
  syn region cobolFormatFree transparent contains=@cobolCode,@cobolFolds
        \ start = '>>\s*SOURCE\s\?FORMAT FREE'
        \ skip  = '^[*/].*$\|\*>.*'
        \ end   = '>>\s*SOURCE\s\?FORMAT FIXED'
endif

" HIGHLIGHTS {{{1

hi def link cobolSeqNum         Tag

hi def link cobolComment        Comment
hi def link cobolInlineComment  cobolComment

hi def link cobolTodo           Todo

hi def link cobolIndicator      SpecialChar
hi def link cobolIndPreProc     cobolIndicator
hi def link cobolIndDebug       cobolIndicator
hi def link cobolIndCont        cobolIndicator

hi def link cobolDebugLine      Debug
hi def link cobolPreProc        PreProc

hi def link cobolConstant       Constant
hi def link cobolNumber         cobolConstant
hi def link cobolString         cobolConstant

hi def link cobolBoolean        Boolean
hi def link cobolBoolOperator   Operator

hi def link cobolArithmetic     Operator

hi def link cobolDataLvl        Type
hi def link cobolPic            Type

hi def link cobolReserved       Statement
hi def link cobolGoTo           Statement
hi def link cobolCalls          Statement

hi def link cobolRepeat         Repeat

hi def link cobolExtra          cobolReserved
hi def link cobolExec           cobolReserved

hi def link cobolConditional    Conditional

hi def link cobolProgIdArea     WarningMsg

hi def link cobolCallProg       Identifier
hi def link cobolFunction       Function

hi def link cobolDivision       Label
hi def link cobolSection        Label
hi def link cobolParagraph      Label

hi def link cobolDivisionName   Keyword
hi def link cobolSectionName    Keyword
hi def link cobolParaNameIdDiv  Keyword
hi def link cobolParaNameSec    Keyword

hi def link cobolCopy           PreProc
hi def link cobolCopyName       Special

hi def link cobolBad            Error
hi def link cobolOver80         cobolBad
