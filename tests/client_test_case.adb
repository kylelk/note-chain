with AUnit.Assertions; use AUnit.Assertions;

with Client;

package body Client_Test_Case is

   overriding function Name
     (Test : Client_Test_Case)
      return Message_String
   is
      pragma Unreferenced (Test);
   begin
      return Format("Client test");
   end Name;

   procedure Test_Create_Branch(T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Test_Client : Client.Client_Status;
      Test_Branch : constant Client.Branch := (+"test", Client.Empty_Hash_Ref);
   begin
      Test_Client.Set_Branch(Test_Branch);
      Assert(Test_Client.Branch_Exists(+"test"), "Branch creation failed");
   end Test_Create_Branch;

   procedure Test_Valid_Branch_Name (T : in out Test_Case'Class) is
      pragma Unreferenced(T);
   begin
      Assert(Client.Valid_Branch_Name("test"), "failed branch name");
   end Test_Valid_Branch_Name;

   procedure Register_Tests (Test : in out Client_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(Test, Test_Create_Branch'Access, "Test create branch");
      Register_Routine(Test, Test_Valid_Branch_Name'Access, "Test branch branch name validation");
   end Register_Tests;

end Client_Test_Case;
