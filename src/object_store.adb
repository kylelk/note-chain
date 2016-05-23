with Ada.Strings.Fixed;
with File_Operations;

package body Object_Store is
   procedure Write
     (db          : in out KV_Store.KV_Container'Class;
      Object_Type :        String;
      Content     :        String;
      ref         :    out SHA256_Value)
   is
      Length_Str : constant String :=
        Ada.Strings.Fixed.Trim (Content'Length'Img, Ada.Strings.Left);
   begin
      declare
         Object_Content : constant String :=
           Object_Type & ' ' & Length_Str & ASCII.LF & Content;
      begin
         ref := File_Operations.String_Hash (Object_Content);
         db.Set (ref, Object_Content);

      end;
   end Write;

   function Read
     (db  : in out KV_Store.KV_Container'Class;
      ref :        SHA256_Value) return String
   is
      File_Content  : constant String := db.Get (ref);
      Newline_Index : Integer;
   begin
      STR_OPS.Check_SHA256 (ref);
      Newline_Index := STR_OPS.Char_Index (File_Content, ASCII.LF) + 1;
      return File_Content (Newline_Index .. File_Content'Last);
   end Read;

   function Read_Object
     (db  : in out KV_Store.KV_Container'Class;
      ref :        SHA256_Value) return String
   is
   begin
      STR_OPS.Check_SHA256 (ref);
      return db.Get (ref);
   end Read_Object;

   function Object_Type
     (Db  : in out KV_Store.KV_Container'Class;
      ref :        SHA256_Value) return String
   is
      File_Content  : constant String := Read_Object (Db, ref);
      Newline_Index : Integer;
      Last_Space    : Integer;
      use STR_OPS;
   begin
      Newline_Index := Char_Index (File_Content, ASCII.LF);
      Last_Space    := Last_Index (File_Content (1 .. Newline_Index), ' ');
      return File_Content (1 .. Last_Space - 1);
   end Object_Type;

   function Exists
     (Db  : in out KV_Store.KV_Container'Class;
      ref :        SHA256_Value) return Boolean
   is
   begin
      STR_OPS.Check_SHA256 (ref);
      return Db.Exists (ref);
   end Exists;
end Object_Store;
