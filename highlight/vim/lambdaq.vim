" Vim syntax file
" Language: lambdaq
" Maintainer: radu.marginean@transilvania-quantum.com
" Latest Revision: 9 August 2023

if exists("b:current_syntax")
  finish
endif

syn match codeGate " H " 
syn match codeGate " X " 
syn match codeGate " Y " 
syn match codeGate " Z " 
syn match codeGate " ID " 
syn match codeGate " ROOT_X " 
syn match codeGate " ROOT_X_DAG " 
syn match codeGate " ROOT_Y " 
syn match codeGate " ROOT_Y_DAG " 
syn match codeGate " ROOT_Z " 
syn match codeGate " ROOT_Z_DAG " 
syn match codeGate "  S " 
syn match codeGate " S_DAG " 
syn match codeGate " T " 
syn match codeGate " T_DAG " 
syn match codeGate " SQRT_X " 
syn match codeGate " SQRT_X_DAG " 
syn match codeGate " SQRT_Y " 
syn match codeGate " SQRT_Y_DAG " 
syn match codeGate " RX " 
syn match codeGate " RY " 
syn match codeGate " RZ " 
syn match codeGate " U1 " 
syn match codeGate " U2 " 
syn match codeGate " U3 " 
syn match codeGate " SWAP " 
syn match codeGate " SQRT_SWAP " 
syn match codeGate " SQRT_SWAP_DAG " 
syn match codeGate " ISWAP " 
syn match codeGate " FSWAP " 
syn match codeGate " SWAP_THETA " 
syn match codeGate " ROOT_SWAP " 
syn match codeGate " ROOT_SWAP_DAG "

syn keyword codeTodo contained TODO FIXME
syn match codeComment "--.*$" contains=codeTodo
syn match codeComment "/\*\{-}\_.\{-}\*/" contains=codeTodo

" Integers
syn match codeInt '\d\+'

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

syn keyword codeBuiltin if then else let in with ctrl gate

syn keyword codeSpecialFunction new measr

syn keyword codeType Bit Qbit 

syn match codeOperator "*"
syn match codeOperator "->"
syn match codeOperator "\["
syn match codeOperator "\]"
syn match codeOperator "="
syn match codeOperator "<-"
syn match codeOperator "\."
syn match codeOperator "!"
syn match codeOperator "\$"

syn match codeControl "@0"
syn match codeControl "@1"
syn match codeControl "@+i"
syn match codeControl "@-i"
syn match codeControl "@+"
syn match codeControl "@-"

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
hi def link codeSpecialFunction Special
