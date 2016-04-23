package KV_Store is
   type KV_Container is interface;

   subtype SHA256_Value is String (1 .. 64);

   No_Key_Error : exception;

   function Name(Self : KV_Container) return String is abstract;

   procedure Setup (self : in out KV_Container) is abstract;

   procedure Cleanup (Self : in out KV_Container) is abstract;

   procedure Commit (Self : in out KV_Container) is abstract;

   procedure Set
     (Self  : in out KV_Container;
      Key   :        SHA256_Value;
      Value :        String) is abstract;

   function Get
     (Self : in out KV_Container;
      Key  :        SHA256_Value) return String is abstract;

   function Exists
     (Self : in out KV_Container;
      Key  : SHA256_Value) return Boolean is abstract;

   procedure Remove
     (Self : in out KV_Container;
      Key  :        SHA256_Value) is abstract;
end KV_Store;
