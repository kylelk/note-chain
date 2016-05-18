with GNAT.Calendar.Time_IO;
with Ada.Calendar.Formatting;
with Ada.Numerics.Discrete_Random;
with GNAT.Regpat;

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
      if not Valid_SHA256(Hash) then
         raise Invalid_Hash_Format;
      end if;
   end Check_SHA256;

   function Valid_SHA256 (Hash : SHA256_Value) return Boolean is
   begin
      for C of Hash loop
         if C not in '0' .. '9' | 'a' .. 'f' then
            return False;
         end if;
      end loop;
      return True;
   end Valid_SHA256;

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

   function Valid_Branch_Name (Name : String) return Boolean is
      package Pat renames GNAT.Regpat;
      Valid_Pattern : constant String := "^[A-Za-z0-9_\-\+\(\)\.]+$";
   begin
      if Name'Length = 40 then
         return False;
      end if;

      if Pat.Match ("\.\.", Name) then
         return False;
      end if;

      return Pat.Match (Valid_Pattern, Name);
   end Valid_Branch_Name;

   function LCS (S1 : String; S2 : String) return LCS_Matrix is
      M      : constant Integer                    := S1'Length;
      N      : constant Integer                    := S2'Length;
      Result : LCS_Matrix (1 .. N + 1, 1 .. M + 1) :=
        (others => (others => 0));
   begin
      for I in 1 .. M + 1 loop
         for J in 1 .. N + 1 loop
            if S1 (I - 1) = S2 (J - 1) then
               Result (I, J) := Result (I - 1, J - 1) + 1;
            else
               Result (I, J) :=
                 Integer'Max (Result (I, J - 1), Result (I - 1, J));
            end if;
         end loop;
      end loop;
      return Result;
   end LCS;
end String_Operations;
