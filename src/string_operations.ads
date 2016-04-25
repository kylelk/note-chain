with Ada.Calendar;

package String_Operations is
   Invalid_Hash_Format : exception;

   subtype SHA256_Value is String (1 .. 64);

   function Char_Index (Data : String; Char : Character) return Integer;

   function Last_Index (Data : String; Char : Character) return Integer;

   function To_ISO_8601 (Date : Ada.Calendar.Time) return String;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time;

   -- checks if a string is 64 lower case hexadecimal characters
   procedure Check_SHA256 (Hash : SHA256_Value);

   function Random_SHA256 return SHA256_Value;
end String_Operations;
