-- @summary
-- Interface for key value data stores

-- @description
-- this is the interface which all new key value data stores must implement
-- keys will be a SHA-256 hash of the value
-- values will be a string
-- when a key cannot be found such as when getting a value or removing a key
-- then a No_Key_Error should be raised

package KV_Store is
   type KV_Container is interface;

   subtype SHA256_Value is String (1 .. 64);

   -- raised when a key cannot be found
   No_Key_Error : exception;

   -- @summary Name of the data store
   -- @description
   -- A unique name used to indentify the data store implementation
   function Name(Self : KV_Container) return String is abstract;

   -- @summary called when starting up
   procedure Setup (self : in out KV_Container) is abstract;

   -- @summary called when quiting
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
