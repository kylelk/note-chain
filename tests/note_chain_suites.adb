with Settings_Test_Case; use Settings_Test_Case;
with Client_Test_Case; use Client_Test_Case;

package body Note_Chain_Suites is
   use Test_Suites;

   Settings_Tests : aliased Settings_Test_Case.Settings_Test_Case;
   Client_Tests : aliased Client_Test_Case.Client_Test_Case;

   function Suite return Test_Suites.Access_Test_Suite is
      Result : constant Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Settings_Tests'Access);
      Result.Add_Test (Client_Tests'Access);

      return Result;
   end Suite;

end Note_Chain_Suites;
