with KV_Store;
with GNAT.Directory_Operations;
with Ada.Text_IO;



package File_Object_Store is
   subtype SHA256_Value is String(1..64);

   Object_Not_Found : exception;
   Invalid_Hash_Format : exception;

   type Data is new KV_Store.KV_Container with null record;

   procedure Setup (self : in out Data);

   procedure Cleanup (self : in out Data);

   procedure Set (Self : in out Data; Key : SHA256_Value ; Value : String);

   function Get (Self : in out Data; Key : SHA256_Value) return String;

   function Contains (Self : Data; Key : SHA256_Value) return Boolean;

   procedure Remove (Self : in out Data; Key : SHA256_Value);

   procedure Commit (Self : in out Data);

private
   package TIO renames Ada.Text_IO;

   function Object_Path(Hash : SHA256_Value) return String;

   function Get_Content (File_Path : String) return String;

   function Char_Index (Data : String; Char : Character) return Integer;

   function Last_Index(Data : String; Char : Character) return Integer;

   procedure Check_SHA256(Hash : SHA256_Value);

   package DIR_OPS renames GNAT.Directory_Operations;

end File_Object_Store;
