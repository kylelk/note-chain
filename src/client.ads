with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

package Client is
   package JSON renames GNATCOLL_JSON;
   package UBS renames Ada.Strings.Unbounded;

   subtype SHA256_Value is String(1..64);
   Empty_Hash_Ref : constant SHA256_Value := (others=>' ');

   type Commit is record
      Object_Ref : SHA256_Value := Empty_Hash_Ref;
      Parent_Ref : SHA256_Value := Empty_Hash_Ref;
      Tree_Ref : SHA256_Value := Empty_Hash_Ref;
   end record;

   type Branch is record
      Name : UBS.Unbounded_String;
      Commit_Ref : SHA256_Value := Empty_Hash_Ref;
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

   -- @description
   -- Current status of the client
   type Client_Status is tagged record
      Branch_Status : Branch_Info;
      -- first time starting the client
      First_Load : Boolean := False;
   end record;

   No_Branch_Error : exception;

   procedure Init(Status : in out Client_Status);

   -- @description
   -- Load branches from the JSON file defined in the config.ads
   procedure Load_Branches(Status : in out Client_Status);

   procedure Set_Head(Status : in out Client_Status; Branch_Name : UBS.Unbounded_String);

   -- @description
   -- Set the Value of a branch
   procedure Set_Branch(Status : in out Client_Status; Item : Branch);

   procedure Copy_Branch(Status : in out Client_Status; From, To : UBS.Unbounded_String);

   -- @description
   -- Save branches to the JSON file defined in config.ads
   procedure Save_Branches(Status : in out Client_Status);
end Client;
