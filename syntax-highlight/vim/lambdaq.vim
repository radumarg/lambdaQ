
" Language: lambdaq
" Maintainer: radu.marginean@transilvania-quantum.com
" Latest Revision: 17 August 2023

if exists("b:current_syntax")
  finish
endif

syn match codeGate '\s[A-Z][A-Z0-9]*\s'
syn match codeGate '\s[A-Z][A-Z0-9]*\n'
syn match codeGate '\s[A-Z][A-Z0-9]*\t'
syn match codeGate '\n[A-Z][A-Z0-9]*\s'
syn match codeGate '\n[A-Z][A-Z0-9]*\n'
syn match codeGate '\n[A-Z][A-Z0-9]*\t'
syn match codeGate '\t[A-Z][A-Z0-9]*\s'
syn match codeGate '\t[A-Z][A-Z0-9]*\n'
syn match codeGate '\t[A-Z][A-Z0-9]*\t'
syn match codeGate '^[A-Z][A-Z0-9]*\s'
syn match codeGate '^[A-Z][A-Z0-9]*\n'
syn match codeGate '^[A-Z][A-Z0-9]*\t'

syn keyword codeTodo contained TODO FIXME
syn match codeComment "--.*$" contains=codeTodo
syn match codeComment "{-\{-}\_.\{-}-}" contains=codeTodo

" Integers
syn match codeInt '\W\zs\d\+\ze\W'
syn match codeInt '\W\zs\d\+$'
syn match codeInt '^\d\+\ze\W'
syn match codeInt '^\d\+$'


" Floating point number with decimal no E or e (+,-)
syn match codeFloat '\d\+\.\d*'
syn match codeFloat '[-+]\d\+\.\d*'

" Floating point like number with E and no decimal point (+,-)
syn match codeFloat '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match codeFloat '\d[[:digit:]]*[eE][\-+]\=\d\+'

" Floating point like number with E and decimal point (+,-)
syn match codeFloat '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match codeFloat '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'

syn region codeString start='"' end='"'

syn keyword codeBuiltin if then else let in with ctrl

syn keyword codeType Bit Qbit State Unitary

syn match codeType "!"
syn match codeType "->"
syn match codeType "\*\*\s\+\d\+"
syn match codeType "\s\+\*\s\+"

syn match codeOperator "\["
syn match codeOperator "\]"
syn match codeOperator "="
syn match codeOperator "<-"
syn match codeOperator "\."
syn match codeOperator "\$"
syn match codeOperator ";"
syn match codeOperator "::"

syn match codeSpecial "@0"
syn match codeSpecial "@1"
syn match codeSpecial "@+i"
syn match codeSpecial "@-i"
syn match codeSpecial "@+"
syn match codeSpecial "@-"

let b:current_syntax = "lambdaq"

hi def link codeTodo Todo 
hi def link codeComment Comment
hi def link codeGate Operator
hi def link codeString String
hi def link codeFloat Constant
hi def link codeInt Constant
hi def link codeType Type
hi def link codeOperator Operator
hi def link codeControl Constant
hi def link codeBuiltin Define
hi def link codeSpecial Special
