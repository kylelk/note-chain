package KV_Store is
   type KV_Container is interface;
   No_Key_Error : exception;

   procedure Setup (self : in out KV_Container) is abstract;

   procedure Cleanup (Self : in out KV_Container) is abstract;

   procedure Commit (Self : in out KV_Container) is abstract;

   procedure Set (Self : in out KV_Container; Key, Value : String) is abstract;

   function Get (Self : in out KV_Container; Key : String) return String is abstract;

   function Contains(Self : KV_Container; Key : String) return Boolean is abstract;

   procedure Remove(Self : in out KV_Container; Key : String) is abstract;
end KV_Store;
