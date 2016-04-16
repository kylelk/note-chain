with AUnit; use AUnit;
with AUnit.Simple_Test_Cases;

package Settings_Test_Case is
   type Settings_Test_Case is new AUnit.Simple_Test_Cases.Test_Case with null record;


   overriding
   function Name (Test : Settings_Test_Case) return Message_String;

   overriding
   procedure Run_Test (Test : in out Settings_Test_Case);
end Settings_Test_Case;
