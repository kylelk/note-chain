with GNATCOLL_JSON;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;
with Ada.Calendar;
with Ada.Text_IO;

with Settings;
with KV_Store;
with String_Operations;

generic
   type Data_Store is new KV_Store.KV_Container with private;
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

   type JSON_Serializable is interface;
   procedure To_JSON
     (Item   : in     JSON_Serializable;
      Result :    out JSON.JSON_Value) is abstract;
   procedure From_JSON
     (Item : in out JSON_Serializable;
      Data :        String) is abstract;

   -- Stores note infomation
   -- @field Note_Text content of the note
   -- @field Encoding text encoding of the note content
   -- @field Uniq_UUID unique note identifier
   -- @field Created_At when the first note version was created
   -- @field Updated_At when this version of the note was created
   -- @field Next_Ref reference to the next note version
   type Note is new Object_Record and JSON_Serializable with record
      Note_Text  : UBS.Unbounded_String;
      Encoding   : UBS.Unbounded_String;
      Uniq_UUID  : SHA256_Value;
      Created_At : Ada.Calendar.Time;
      Updated_At : Ada.Calendar.Time;
      Next_Ref   : SHA256_Value         := Empty_Hash_Ref;
      Author     : UBS.Unbounded_String := UBS.Null_Unbounded_String;
   end record;

   type Commit is new Object_Record and JSON_Serializable with record
      Parents    : Reference_Set.Set;
      Tree_Ref   : SHA256_Value         := Empty_Hash_Ref;
      Created_At : Ada.Calendar.Time;
      Message    : UBS.Unbounded_String := UBS.Null_Unbounded_String;
   end record;

   type Tree_Entry is record
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

   type Tree is new Object_Record and JSON_Serializable with record
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
      Data            : Data_Store;
      -- first time starting the client
      First_Load : Boolean := False;
   end record;

   package Object_Store is
      subtype SHA256_Value is String (1 .. 64);

      Object_Not_Found : exception;

      -- @description
      -- Store an object's content
      procedure Write
        (Status      : in out Client_Status'Class;
         Object_Type :        String;
         Content     :        String;
         Hash        :    out SHA256_Value);

      -- @description
      -- Get the content of an object, raises Object_Not_Found when a object
      -- does not exist
      function Read
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return String;

      -- @description
      -- returns the entire data object as a string
      function Read_Object
        (Status : Client_Status'Class;
         Hash   : SHA256_Value) return String;

      function Object_Type
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return String;

      -- @description
      -- checks if an object exists
      function Exists
        (Status : in out Client_Status'Class;
         Hash   :        SHA256_Value) return Boolean;

   private
      -- @private
      package TIO renames Ada.Text_IO;
   end Object_Store;

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
   procedure Create_Note
     (Status       : in out Client_Status;
      Item         :    out Note'Class;
      Note_Content :        String);

   -- @summary Adds a note to a tree
   procedure Add_Note (T : in out Tree; Note_Entry : Note'Class);

   procedure To_JSON (Item : in Note; Result : out JSON.JSON_Value);
   procedure To_JSON (Item : in Tree; Result : out JSON.JSON_Value);
   procedure To_JSON (Item : in Commit; Result : out JSON.JSON_Value);

   procedure From_JSON (Item : in out Note; Data : String);
   procedure From_JSON (Item : in out Tree; Data : String);
   procedure From_JSON (Item : in out Commit; Data : String);

   -- @summary
   -- saves a Note to the object store
   procedure Save (Status : in out Client_Status; Item : in out Note'Class);

   -- @summary
   -- saves a Tree_Entry to the object store
   procedure Save (Status : in out Client_Status; Item : in out Tree'Class);

   -- @summary
   -- saves a Commit to the object store
   procedure Save (Status : in out Client_Status; Item : in out Commit'Class);

   function Get_Commit
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Commit;

   function Get_Tree
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Tree;

   function Get_Note
     (Status : in out Client_Status'Class;
      Ref    :        SHA256_Value) return Note;

   -- @summary
   -- get commit SHA-256 of the commit for the current head branch
   function Head_Commit_Ref (Status : Client_Status) return SHA256_Value;

   -- @summary
   -- returns the commit at the head of the current branch
   function Head_Commit (Status : in out Client_Status) return Commit'Class;

   -- @summary
   -- returns the head branch
   function Head (Status : Client_Status) return Branch;

   procedure Set_Head_Ref (Status : in out Client_Status; Ref : SHA256_Value);

   function Branch_Exists
     (Status      : Client_Status;
      Branch_Name : UBS.Unbounded_String) return Boolean;

   -- @summary get a branch by it's name
   function Get_Branch
     (Status : Client_Status;
      Name   : UBS.Unbounded_String) return Branch;

   -- @summary
   -- updates the current branch head to a commit
   procedure Set_Head (Status : in out Client_Status; Item : Commit'Class);

   procedure Tree_Refs
     (Status     : in out Client_Status;
      Start_Ref  :        SHA256_Value;
      References : in out Reference_Set.Set);

   procedure Branch_Refs
     (Status     : in out Client_Status;
      Item       :        Branch;
      References : in out Reference_Set.Set);

   procedure Export (Status : in out Client_Status; Filename : String);

   procedure Export_Refs
     (Status   : in out Client_Status;
      Items    :        Reference_Set.Set;
      Filename :        String);

   function Format_Note (Item : Note) return String;

   -- @summary
   -- Traverse the commits and pass each commit to a procedure
   procedure Traverse_Commits
     (Status : in out Client_Status;
      Ref    :        SHA256_Value;
      Proc   :        access procedure (Item : Commit));

   -- @summary joins together the enties of both trees
   function Join_Trees (Left, Right : Tree) return Tree;

   -- @summary
   -- Merges two branches together
   -- @description
   -- merges branch B into branch A and creates a new commit for the merge
   procedure Merge_Branches
     (Status : in out Client_Status;
      A      : in out Branch;
      B      :        Branch);

private
   package STR_OPS renames String_Operations;
end Client;
