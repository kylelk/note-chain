with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Calendar;

package Client is
   package JSON renames GNATCOLL_JSON;
   package UBS renames Ada.Strings.Unbounded;

   subtype SHA256_Value is String (1 .. 64);
   Empty_Hash_Ref : constant SHA256_Value := (others => ' ');

   type Object_Type is (Type_Commit, Type_Tree, Type_Note, Type_Blob);

   type Note is record
      Object_Ref : SHA256_Value := Empty_Hash_Ref;
      Note_Text  : UBS.Unbounded_String;
      Encoding   : UBS.Unbounded_String;
      Uniq_UUID  : SHA256_Value;
      Created_At : Ada.Calendar.Time;
      Saved : Boolean := False;
   end record;

   type Commit is record
      Object_Ref : SHA256_Value := Empty_Hash_Ref;
      Parent_Ref : SHA256_Value := Empty_Hash_Ref;
      Tree_Ref   : SHA256_Value := Empty_Hash_Ref;
      Created_At : Ada.Calendar.Time;
   end record;

   type Tree_Entry is record
      Object_Ref : SHA256_Value := Empty_Hash_Ref;
      Entry_Type : Object_Type range Type_Tree .. Type_Note;
      Child_Ref  : SHA256_Value := Empty_Hash_Ref;
      Next_Ref   : SHA256_Value := Empty_Hash_Ref;
      Name       : UBS.Unbounded_String;
   end record;

   type Branch is record
      Name       : UBS.Unbounded_String;
      Commit_Ref : SHA256_Value := Empty_Hash_Ref;
   end record;

   function Hash
     (Key : UBS.Unbounded_String) return Ada.Containers.Hash_Type is
     (UBS.Hash (Key));

   package Branch_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => UBS.Unbounded_String,
      Element_Type    => Branch,
      Hash            => Hash,
      Equivalent_Keys => UBS."=");

   type Branch_Info is record
      Head     : UBS.Unbounded_String := UBS.Null_Unbounded_String;
      Branches : Branch_Map.Map;
   end record;

   -- Current status of the client
   type Client_Status is tagged record
      Branch_Status : Branch_Info;
      -- first time starting the client
      First_Load : Boolean := False;
   end record;

   No_Branch_Error : exception;

   procedure Init (Status : in out Client_Status);

   -- @description
   -- Load branches from the JSON file defined in the config.ads
   procedure Load_Branches (Status : in out Client_Status);

   procedure Set_Head
     (Status      : in out Client_Status;
      Branch_Name :        UBS.Unbounded_String);

   -- @description
   -- Set the Value of a branch
   procedure Set_Branch (Status : in out Client_Status; Item : Branch);

   procedure Copy_Branch
     (Status   : in out Client_Status;
      From, To :        UBS.Unbounded_String);

   -- @description
   -- Save branches to the JSON file defined in config.ads
   procedure Save_Branches (Status : in out Client_Status);

   procedure Create_Note (Status : in out Client_Status; Item : out Note);

   procedure Save(Status : in out Client_Status; Item : in out Note);

   function Get_Commit(Ref : SHA256_Value) return Commit;

   function Get_Tree_Entry(Ref : SHA256_Value) return Tree_Entry;

   function Get_Note(Ref : SHA256_Value) return Note;

   -- get commit SHA-256 of the commit for the current head branch
   function Head_Commit_Ref(Status : Client_Status) return SHA256_Value;

private
   function Random_SHA256 return SHA256_Value;

   function To_ISO_8601(Date : Ada.Calendar.Time) return String;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time;
end Client;
