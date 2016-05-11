package body File_Assets is
   function Vim_Select_Line_Script return String is
   begin
      return "function! Select_Line()" &
        "let current_line=getline('.')" &
        "2b" &
        "%d" &
        "put =current_line" &
        "v/./d" &
        "write" &
        "q!" &
        "endfunction" &
        "set cursorline" &
        "map <CR> :call Select_Line()<CR>";
   end Vim_Select_Line_Script;
end File_Assets;
