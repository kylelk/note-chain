package body String_Operations is
    function Char_Index (Data : String; Char : Character) return Integer is
      begin
         for I in Data'Range loop
            if Data (I) = Char then
               return I;
            end if;
         end loop;
         return -1;
      end Char_Index;

      function Last_Index (Data : String; Char : Character) return Integer is
      begin
         for I in reverse Data'Range loop
            if Data (I) = Char then
               return I;
            end if;
         end loop;
         return -1;
      end Last_Index;

      procedure Check_SHA256 (Hash : SHA256_Value) is
      begin
         for C of Hash loop
            if C not in '0' .. '9' | 'a' .. 'f' then
               raise Invalid_Hash_Format;
            end if;
         end loop;
      end Check_SHA256;
end String_Operations;
