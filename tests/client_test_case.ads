with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded;

package Client_Test_Case is
   type Client_Test_Case is new AUnit.Test_Cases.Test_Case with null record;
   package UBS renames Ada.Strings.Unbounded;

   function "+"(S : String) return UBS.Unbounded_String
   is (UBS.To_Unbounded_String(S));

   overriding
   function Name (Test : Client_Test_Case) return Message_String;

   procedure Test_Create_Branch (T : in out Test_Case'Class);

   procedure Test_Valid_Branch_Name (T : in out Test_Case'Class);

   procedure Register_Tests( Test : in out Client_Test_Case );

end Client_Test_Case;
