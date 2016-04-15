with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

package Message_Format is
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

   type Message is tagged record
      Headers : KV_Map.Map;
      Content : UBS.Unbounded_String;
   end record;

   procedure Set_Header(Mesg : in out Message; Key, Value : String);

   procedure Set_Header(Mesg : in out Message; Key, Value : Unbounded_String);

   procedure Set_Content(Mesg : in out Message; Content : UBS.Unbounded_String);

   procedure Set_Content(Mesg : in out Message; Content : String);

   function To_String(Mesg : in out Message) return String;

end Message_Format;
