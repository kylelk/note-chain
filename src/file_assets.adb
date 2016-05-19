package body File_Assets is
   function Vim_Select_Line_Script return String is
      NL : constant Character := ASCII.LF;
      I : constant String := "   ";
   begin
      return "" &
        "function! Select_Line()" & NL &
        I & "let current_line=getline('.')" & NL &
        I & "2b" & NL &
        I & "%d" & NL &
        I & "put =current_line" & NL &
        I & "v/./d" & NL &
        I & "write" & NL &
        I & "q!" & NL &
        "endfunction" & NL &
        "set cursorline" & NL &
        "set nospell" & NL &
        "set nonumber" & NL &
        "hi CursorLine cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white" & NL &
        "1" & NL &
        "map <CR> :call Select_Line() <CR>";
   end Vim_Select_Line_Script;
end File_Assets;
