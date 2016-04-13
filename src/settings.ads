with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package Settings is
   package UBS renames Ada.Strings.Unbounded;


   function Hash(Key : UBS.Unbounded_String)
                 return Ada.Containers.Hash_Type is (UBS.Hash (Key));


   use UBS;
   package KV_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => UBS.Unbounded_String,
      Element_Type    => UBS.Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => UBS."=");

   type Settings_Data is tagged record
      Values : KV_Map.Map;
      Modified : Boolean := False;
   end record;

   No_Key_Error : exception;

   procedure Load(Data : out Settings_Data);

   function Exists(Data : in out Settings_Data; Key : String) return Boolean;

   function Get(Data : in out Settings_Data; Key : String) return String;

   procedure Set(Data : in out Settings_Data; Key : String; Value : String);

   procedure Save(Data : Settings_Data);
end Settings;
