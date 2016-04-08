with Ada.Text_IO;
with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;


package Client is
   package TIO renames Ada.Text_IO;
   package JSON renames GNATCOLL_JSON;
   package UBS renames Ada.Strings.Unbounded;

   subtype SHA256_Value is String(1..64);
   Empty_Tree_Ref : constant SHA256_Value := (others=>' ');
   type Branch is record
      Name : UBS.Unbounded_String;
      Tree_Ref : SHA256_Value := Empty_Tree_Ref;
   end record;

   function Hash (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is
    (UBS.Hash(Key));

   package Branch_Map is new Ada.Containers.Hashed_Maps
     (Key_Type     => UBS.Unbounded_String,
      Element_Type => Branch,
      Hash => Hash,
      Equivalent_Keys => UBS."=");

   type Branch_Info is record
      Head : UBS.Unbounded_String := UBS.Null_Unbounded_String;
      Branches : Branch_Map.Map;
   end record;

   type Client_Status is tagged record
      Branch_Status : Branch_Info;
      Default_Branch : Branch;
      First_Load : Boolean := False;
   end record;

   procedure Load_Branches(Status : in out Client_Status);
   procedure Set_Head(Status : in out Client_Status; Branch_Name : UBS.Unbounded_String );
   procedure Set_Branch(Status : in out Client_Status; Item : Branch);
   procedure Save_Branches(Status : in out Client_Status);
end Client;
