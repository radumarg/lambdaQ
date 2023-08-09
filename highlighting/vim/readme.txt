
 1) Install the syntax by copying the file to ~/.vim/syntax/lambdaq.vim on Unix-based systems, or to $HOME/vimfiles/syntax/lambdaq.vim on Windows systems.
 2) In order to make vim recognize the file, add next line of code to: ~/.vimrc 
    
   au bufreadpre,bufnewfile *.lq set ft=lambdaq
