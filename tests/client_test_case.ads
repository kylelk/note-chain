with AUnit; use AUnit;
with AUnit.Test_Cases;

package Client_Test_Case is
   type Client_Test_Case is new AUnit.Test_Cases.Test_Case with null record;


   overriding
   function Name (Test : Client_Test_Case) return Message_String;

   procedure Register_Tests( Test : in out Client_Test_Case );

end Client_Test_Case;
