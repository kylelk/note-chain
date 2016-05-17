with AUnit;            use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded;

with Hash_Store;
with String_Operations;

package Client_Test_Case is
   type Client_Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   Db : Hash_Store.Data;

   -- @private
   function "+"
     (S : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   overriding function Name (Test : Client_Test_Case) return Message_String;

   procedure Test_Create_Branch (T : in out Test_Case'Class);

   procedure Test_Valid_Branch_Name (T : in out Test_Case'Class);

   procedure Test_Create_Note (T : in out Test_Case'Class);

   procedure Test_Get_Note (T : in out Test_Case'Class);

   procedure Test_Create_Tree (T : in out Test_Case'Class);

   procedure Test_Create_Commit (T : in out Test_Case'Class);

   procedure Register_Tests (Test : in out Client_Test_Case);
private
   package STR_OPS renames String_Operations;
end Client_Test_Case;
