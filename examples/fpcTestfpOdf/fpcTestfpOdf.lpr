program fpcTestfpOdf;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, tst_fpOdfBasics;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

