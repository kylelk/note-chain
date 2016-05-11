function! Select_Line()
    let current_line=getline('.')
    2b
    %d
    put =current_line
    v/./d
    write
    q!
endfunction

set cursorline
map <CR> :call Select_Line()<CR>
