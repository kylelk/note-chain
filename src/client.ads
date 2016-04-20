with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;
with Ada.Calendar;

with Settings;

package Client is
   -- @private
   package JSON renames GNATCOLL_JSON;
   -- @private
   package UBS renames Ada.Strings.Unbounded;

   subtype SHA256_Value is String (1 .. 64);
   Empty_Hash_Ref : constant SHA256_Value := (others => ' ');

   type Object_Type is (Type_Commit, Type_Tree, Type_Note, Type_Blob);

   package Reference_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => SHA256_Value,
      Hash                => Ada.Strings.Hash,
      Equivalent_Elements => "=");

   -- @field Object_Ref data object which contains the record
   -- @field Saved has the record been saved yet
   type Object_Record is abstract tagged record
      Object_Ref : SHA256_Value := Empty_Hash_Ref;
      Saved      : Boolean      := False;
   end record;

   -- Stores note infomation
   -- @field Note_Text content of the note
   -- @field Encoding text encoding of the note content
   -- @field Uniq_UUID unique note identifier
   -- @field Created_At when the first note version was created
   -- @field Updated_At when this version of the note was created
   -- @field Next_Ref reference to the next note version
   -- @field Version version number of the note edit
   type Note is new Object_Record with record
      --Object_Ref : SHA256_Value         := Empty_Hash_Ref;
      Note_Text  : UBS.Unbounded_String;
      Encoding   : UBS.Unbounded_String;
      Uniq_UUID  : SHA256_Value;
      Created_At : Ada.Calendar.Time;
      Updated_At : Ada.Calendar.Time;
      Next_Ref   : SHA256_Value         := Empty_Hash_Ref;
      Version    : Integer              := 1;
      Author     : UBS.Unbounded_String := UBS.Null_Unbounded_String;
   end record;

   type Commit is new Object_Record with record
      Parents    : Reference_Set.Set;
      Tree_Ref   : SHA256_Value         := Empty_Hash_Ref;
      Created_At : Ada.Calendar.Time;
      Message    : UBS.Unbounded_String := UBS.Null_Unbounded_String;
   end record;

   type Tree_Entry is record
      -- Object_Ref : SHA256_Value         := Empty_Hash_Ref;
      Entry_Type : Object_Type range Type_Tree .. Type_Note;
      Child_Ref  : SHA256_Value         := Empty_Hash_Ref;
      Name       : UBS.Unbounded_String := UBS.Null_Unbounded_String;
   end record;

   function Tree_Entry_Hash
     (Item : Tree_Entry) return Ada.Containers.Hash_Type;

   package Tree_Entry_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => Tree_Entry,
      Hash                => Tree_Entry_Hash,
      Equivalent_Elements => "=");

   type Tree is new Object_Record with record
      Entries : Tree_Entry_Set.Set;
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
      Branch_Status   : Branch_Info;
      Settings_Status : Settings.Settings_Data;
      -- first time starting the client
      First_Load : Boolean := False;
   end record;

   No_Branch_Error : exception;
   Branch_Name_Format_Error : exception;

   procedure Init (Status : in out Client_Status);

   -- @summary
   -- called when done with the client
   -- @description
   -- saves the branches, cleans up the temporary directory
   procedure Cleanup (Status : in out Client_Status);

   -- @description
   -- Load branches from the JSON file defined in the config.ads
   procedure Load_Branches (Status : in out Client_Status);

   procedure Checkout_Branch
     (Status      : in out Client_Status;
      Branch_Name :        UBS.Unbounded_String);

   -- @description
   -- Set the Value of a branch
   procedure Set_Branch (Status : in out Client_Status; Item : Branch);

   -- @description
   -- creates a new branch from another, when no branch exists then a
   -- No_Branch_Error exception is raised
   procedure Copy_Branch
     (Status   : in out Client_Status;
      From, To :        UBS.Unbounded_String);

   -- @description
   -- Save branches to the JSON file defined in config.ads
   procedure Save_Branches (Status : in out Client_Status);

   -- @summary initalizes a new note object
   -- @description
   -- creates a new note object with the contents of the temporary note file
   procedure Create_Note
     (Status : in out Client_Status;
      Item   :    out Note'Class);

   procedure Add_Note (T : in out Tree; Note_Entry : Note'Class);

   -- @summary
   -- saves a Note to the object store
   procedure Save (Status : in out Client_Status; Item : in out Note'Class);

   -- @summary
   -- saves a Tree_Entry to the object store
   procedure Save (Status : in out Client_Status; Item : in out Tree'Class);

   -- @summary
   -- saves a Commit to the object store
   procedure Save (Status : in out Client_Status; Item : in out Commit'Class);

   function Get_Commit (Ref : SHA256_Value) return Commit;

   function Get_Tree (Ref : SHA256_Value) return Tree;

   function Get_Note (Ref : SHA256_Value) return Note;

   -- @summary
   -- get commit SHA-256 of the commit for the current head branch
   function Head_Commit_Ref (Status : Client_Status) return SHA256_Value;

   -- @summary
   -- returns the commit at the head of the current branch
   function Head_Commit (Status : Client_Status) return Commit'Class;

   -- @summary
   -- returns the head branch
   function Head (Status : Client_Status) return Branch;

   procedure Set_Head_Ref (Status : in out Client_Status; Ref : SHA256_Value);

   function Branch_Exists
     (Status      : Client_Status;
      Branch_Name : UBS.Unbounded_String) return Boolean;

   -- @summary
   -- updates the current branch head to a commit
   procedure Set_Head (Status : in out Client_Status; Item : Commit'Class);

   procedure Tree_Refs
     (Start_Ref  :        SHA256_Value;
      References : in out Reference_Set.Set);

   procedure Branch_Refs
     (Item       :        Branch;
      References : in out Reference_Set.Set);

   procedure Export (Status : Client_Status; Filename : String);

   procedure Export_Refs (Items : Reference_Set.Set; Filename : String);

   function Format_Note (Item : Note) return String;

   -- @summary validate a branch name
   function Valid_Branch_Name (Name : String) return Boolean;

   -- @summary
   -- Traverse the commits and pass each commit to a procedure
   procedure Traverse_Commits (Ref : SHA256_Value; Proc : access procedure(Item : Commit));

   function Join_Trees(Left, Right : Tree) return Tree;

   function To_ISO_8601 (Date : Ada.Calendar.Time) return String;

   function From_ISO_8601 (Date_Str : String) return Ada.Calendar.Time;

private
   function Random_SHA256 return SHA256_Value;
end Client;
