function! Select_Line()
	let current_line=getline('.')
    %d
    put =current_line
    v/./d
    write! current_line.txt
    q!
endfunction

set cursorline
map <CR> :call Select_Line()<CR>

