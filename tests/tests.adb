with AUnit.Run;
with AUnit.Reporter.Text;
with Note_Chain_Suites;

procedure Unit_Tests is
   procedure Run is new AUnit.Run.Test_Runner (Note_Chain_Suites.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);

   Run (Reporter);
end Unit_Tests;
