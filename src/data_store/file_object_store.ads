with KV_Store;
with GNAT.Directory_Operations;
with Ada.Text_IO;

package File_Object_Store is
   subtype SHA256_Value is String(1..64);

   type Data is new KV_Store.KV_Container with null record;

   function Name(Self : Data) return String is ("file_object_store");

   procedure Setup (self : in out Data);

   procedure Cleanup (self : in out Data);

   procedure Set (Self : in out Data; Key : SHA256_Value ; Value : String);

   function Get (Self : in out Data; Key : SHA256_Value) return String;

   function Exists (Self : in out Data; Key : SHA256_Value) return Boolean;

   procedure Remove (Self : in out Data; Key : SHA256_Value);

   procedure Commit (Self : in out Data);

private
   package TIO renames Ada.Text_IO;

   function Object_Path(Hash : SHA256_Value) return String;

   function Get_Content (File_Path : String) return String;

   package DIR_OPS renames GNAT.Directory_Operations;

end File_Object_Store;
