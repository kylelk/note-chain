with KV_Store;
with String_Operations;

package Object_Store is
   subtype SHA256_Value is String (1 .. 64)
     with Predicate => (String_Operations.Valid_SHA256(SHA256_Value));

      Object_Not_Found : exception;

      -- @description
      -- Store an object's content
      procedure Write
        (db          : in out KV_Store.KV_Container'Class;
         Object_Type :        String;
         Content     :        String;
         ref         :    out SHA256_Value);

      -- @description
      -- Get the content of an object, raises Object_Not_Found when a object
      -- does not exist
      function Read
        (db  : in out KV_Store.KV_Container'Class;
         ref :        SHA256_Value) return String;

      -- @description
      -- returns the entire data object as a string
      function Read_Object
        (db  : in out KV_Store.KV_Container'Class;
         ref :        SHA256_Value) return String;

      function Object_Type
        (Db  : in out KV_Store.KV_Container'Class;
         ref :        SHA256_Value) return String;

      -- @description
      -- checks if an object exists
      function Exists
        (Db  : in out KV_Store.KV_Container'Class;
         ref :        SHA256_Value) return Boolean;

   private
   -- @private
   package STR_OPS renames String_Operations;
   end Object_Store;
