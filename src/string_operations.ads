package String_Operations is
   Invalid_Hash_Format : exception;

   subtype SHA256_Value is String (1 .. 64);

   function Char_Index (Data : String; Char : Character) return Integer;

   function Last_Index (Data : String; Char : Character) return Integer;

   -- checks if a string is 64 lower case hexadecimal characters
   procedure Check_SHA256 (Hash : SHA256_Value);
end String_Operations;
