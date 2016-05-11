function! Select_Line()
	let current_line=getline('.')
    %d
    put =current_line
    v/./d
    write! .note_chain_current_line_v1.txt
    q!
endfunction

set cursorline
map <CR> :call Select_Line()<CR>

