with AUnit; use AUnit;
with AUnit.Test_Cases;

package Settings_Test_Case is
   type Settings_Test_Case is new AUnit.Test_Cases.Test_Case with null record;


   overriding
   function Name (Test : Settings_Test_Case) return Message_String;

   procedure Test_Set_Path (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Set_Value (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Get_Value (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Remove (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Raising_Exception_Get;

   procedure Test_No_Key_Exception_Get (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Raising_Exception_Remove;

   procedure Test_No_Key_Exception_Remove (T : in out Test_Cases.Test_Case'Class);

   procedure Set_Up(Test : in out Settings_Test_Case);

   procedure Tear_Down(Test : in out Settings_Test_Case);

   procedure Register_Tests( Test : in out Settings_Test_Case );
end Settings_Test_Case;
