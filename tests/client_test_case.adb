with AUnit.Assertions; use AUnit.Assertions;

package body Client_Test_Case is
   overriding function Name (Test : Client_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return Format ("Client test");
   end Name;

   procedure Test_Create_Branch (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Client_Status : Test_Client.Client_Status;

      Test_Branch : constant Test_Client.Branch := (+"test", Test_Client.Empty_Hash_Ref);
   begin
      Client_Status.Set_Branch (Test_Branch);
      Assert (Client_Status.Branch_Exists (+"test"), "Branch creation failed");
   end Test_Create_Branch;

   procedure Test_Valid_Branch_Name (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Test_Client.Valid_Branch_Name ("test"), "failed branch name");
      Assert (Test_Client.Valid_Branch_Name ("example.com"), "failed branch name");
      Assert
        (not Test_Client.Valid_Branch_Name ("test..master"),
         "failed branch name");
      Assert
        (not Test_Client.Valid_Branch_Name
           ("2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"),
         "failed branch name");
      Assert
        (not Test_Client.Valid_Branch_Name ("Contains Space"),
         "failed branch name");
   end Test_Valid_Branch_Name;

   procedure Register_Tests (Test : in out Client_Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (Test, Test_Create_Branch'Access, "Test create branch");
      Register_Routine
        (Test,
         Test_Valid_Branch_Name'Access,
         "Test branch name validation");
   end Register_Tests;

end Client_Test_Case;
