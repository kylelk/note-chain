with Ada.Text_IO;

package Object_Store is
   subtype SHA256_Value is String(1..64);

   Object_Not_Found : exception;

   -- @description
   -- Store an object's content
   procedure Write(Object_Type : String; Content : String; Hash : out SHA256_Value);

   -- @description
   -- Get the content of an object, raises Object_Not_Found when a object
   -- does not exist
   function Read(Hash : SHA256_Value) return String;

   procedure Init;

private
   package TIO renames Ada.Text_IO;

   function Object_Path(Hash : SHA256_Value) return String;
end Object_Store;
