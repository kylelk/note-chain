with Ada.Text_IO;

package Object_Store is
   subtype SHA256_Value is String(1..64);

   Object_Not_Found : exception;
   Invalid_Hash_Format : exception;

   -- @description
   -- Store an object's content
   procedure Write(Object_Type : String; Content : String; Hash : out SHA256_Value);

   -- @description
   -- Get the content of an object, raises Object_Not_Found when a object
   -- does not exist
   function Read(Hash : SHA256_Value) return String;

   function Read_Data(Hash : SHA256_Value) return String;

   function Object_Type(Hash : SHA256_Value) return String;

   -- @description
   -- checks if an object exists
   function Exists(Hash : SHA256_Value) return Boolean;

private
   package TIO renames Ada.Text_IO;

   function Object_Path(Hash : SHA256_Value) return String;

   function Get_Content (File_Path : String) return String;

   function Char_Index (Data : String; Char : Character) return Integer;

   function Last_Index(Data : String; Char : Character) return Integer;

   procedure Check_SHA256(Hash : SHA256_Value);
end Object_Store;
