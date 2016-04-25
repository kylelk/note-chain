with GNAT.Calendar.Time_IO;
with Ada.Calendar.Formatting;
with Ada.Numerics.Discrete_Random;

with File_Operations;

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

   function To_ISO_8601 (Date : in Ada.Calendar.Time) return String is
   begin
      return GNAT.Calendar.Time_IO.Image (Date, "%Y-%m-%dT%H:%M:%S");
   end To_ISO_8601;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time is
      Year   : Integer;
      Month  : Integer range 1 .. 12;
      Day    : Integer range 1 .. 31;
      Hour   : Integer range 0 .. 23;
      Minute : Integer range 0 .. 59;
      Second : Integer range 0 .. 59;
   begin
      Year  := Integer'Value (Date_Str (Date_Str'First .. Date_Str'First + 3));
      Month :=
        Integer'Value (Date_Str (Date_Str'First + 5 .. Date_Str'First + 6));
      Day :=
        Integer'Value (Date_Str (Date_Str'First + 8 .. Date_Str'First + 9));
      Hour :=
        Integer'Value (Date_Str (Date_Str'First + 11 .. Date_Str'First + 12));
      Minute :=
        Integer'Value (Date_Str (Date_Str'First + 14 .. Date_Str'First + 15));
      Second :=
        Integer'Value (Date_Str (Date_Str'First + 17 .. Date_Str'First + 18));
      return Ada.Calendar.Formatting.Time_Of
          (Year       => Year,
           Month      => Month,
           Day        => Day,
           Hour       => Hour,
           Minute     => Minute,
           Second     => Second,
           Sub_Second => 0.0);
   end From_ISO_8601;

   procedure Check_SHA256 (Hash : SHA256_Value) is
   begin
      for C of Hash loop
         if C not in '0' .. '9' | 'a' .. 'f' then
            raise Invalid_Hash_Format;
         end if;
      end loop;
   end Check_SHA256;

   function Random_SHA256 return SHA256_Value is
      package Guess_Generator is new Ada.Numerics.Discrete_Random (Character);
      Gen  : Guess_Generator.Generator;
      Data : SHA256_Value;
   begin
      for I in Data'Range loop
         Guess_Generator.Reset (Gen);
         Data (I) := Guess_Generator.Random (Gen);
      end loop;
      return File_Operations.String_Hash (Data);
   end Random_SHA256;
end String_Operations;
