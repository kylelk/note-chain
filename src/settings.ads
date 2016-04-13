with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package Settings is
   package UBS renames Ada.Strings.Unbounded;

   function Hash
     (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is
     (UBS.Hash (Key));

   use UBS;
   package KV_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => UBS.Unbounded_String,
      Element_Type    => UBS.Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => UBS."=");

   type Settings_Data is tagged record
      Values   : KV_Map.Map;
      Modified : Boolean := False;
   end record;

   No_Key_Error : exception;

   -- @summary loads the settings data
   procedure Load (Data : out Settings_Data);

   -- @summary checks if a key exists
   function Exists (Data : in out Settings_Data; Key : String) return Boolean;

   -- @summary get a setting value
   -- @description
   -- Gets a value by it's key name and returns the result as a string
   -- if the key does not exist then a No_Key_Error is raised
   function Get (Data : in out Settings_Data; Key : String) return String;

   -- @summary set a value
   -- @description
   -- Creates a new setting value, if the key already exists it is updated
   procedure Set (Data : in out Settings_Data; Key : String; Value : String);

   -- @summary remove setting key
   -- @description
   -- removes an entry from the settings, if the key does not exist then a
   -- No_Key_Error is raised
   procedure Remove (Data : in out Settings_Data; Key : String);

   -- @summary saves the settings data
   procedure Save (Data : Settings_Data);
end Settings;
